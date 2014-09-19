module Main where

import           Control.Applicative     ((<$>))
import           Control.Monad           (void, when)
import           Data.Char               (isSpace)
import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict     as M
import           Data.List               (intercalate, isInfixOf)
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Time.Clock
import           Data.Time.Format
import           Development.Shake
import           Prelude                 hiding ((++))
import           System.Directory        (createDirectoryIfMissing,
                                          getCurrentDirectory)
import           System.Exit
import           System.IO
import           System.Process

(++) :: Monoid a => a -> a -> a
(++) = mappend

opts :: ShakeOptions
opts = shakeOptions { shakeFiles    = ".shake/" }

getProjectName :: IO String
getProjectName = (reverse . takeWhile (/= '/') . reverse) <$>
                   getCurrentDirectory

exec :: String -> Action ExitCode
exec c = liftIO $ do putStrLn c
                     system c

readExec :: String -> Action String
readExec c = liftIO $ do (_,Just out,_, ph) <- createProcess $ (shell c) { std_out = CreatePipe }
                         waitForProcess ph
                         hGetContents out

getDockerTag proj h env = stripWhitespace <$> readExec ("ssh " ++ h ++ " \"docker ps\" | grep " ++ proj ++ env ++ " | awk '{ print $2}' | cut -d ':' -f 2")

stripWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

main :: IO ()
main = do
  proj <- getProjectName
  conf <- load [Required "Rivetfile"]
  commands <- (map (\(k,String v) -> (T.drop (length "commands.") k, v)) .
               filter (\(k,v) -> (T.pack "commands.") `T.isPrefixOf` k) .
               M.toList) <$> getMap conf
  deps <- lookupDefault [] conf (T.pack "dependencies")
  let depDirs = map ((++ ".d/.rivetclone") . ("deps/" ++) . T.unpack . head .
                     T.splitOn (T.pack ":") . head . T.splitOn (T.pack "+")) deps
  shakeArgsWith opts [] $ \flags targets -> return $ Just $ do
     if null targets
        then return ()
        else if "test" == head targets
             then want ["test"]
             else want targets
     sequence_ (map (\(cName, cCom) -> (T.unpack cName) ~> void (exec (T.unpack cCom)))
                    commands)
     sequence_ (map (\d ->
       do let (repo':rest) = T.splitOn (T.pack "+") d
          let (repo:branchspec) = T.splitOn (T.pack ":") repo'
          let depdir = ("deps/" ++ (T.unpack repo) ++ ".d")
          depdir ++ "/.rivetclone" *> \clonedFile -> do
            liftIO $ removeFiles depdir ["//*"]
            () <- cmd ("git clone https://github.com/"
                       ++ (T.unpack repo) ++ " " ++ depdir)
            writeFile' clonedFile ""
            case branchspec of
              (branch:_) -> cmd (Cwd depdir) ("git checkout " ++ (T.unpack branch))
              _ -> return ()
            let addSource s = do let str = "cabal sandbox add-source " ++ s
                                 liftIO $ appendFile "deps/add-all" (str ++ "\n")
                                 cmd str :: Action ()
            case rest of
              (subdirs:_) -> mapM_ (\subdir -> addSource (depdir ++ "/" ++ (T.unpack subdir)))
                                   (T.splitOn (T.pack ",") subdirs)
              _ -> addSource depdir)
        deps)
     let binary = "./.cabal-sandbox/bin/" ++ proj
     binary *> \_ -> do files <- getDirectoryFiles "" ["src/Main.hs", "*.cabal"]
                        need files
                        cmd "cabal install -fdevelopment --reorder-goals --force-reinstalls"
     "run" ~> do need [binary]
                 void $ exec binary
     "run:docker" ~> do exec "ln -sf docker/Dockerfile.development Dockerfile"
                        exec $ "sudo docker build -t " ++ proj ++ "_devel ."
                        exec "rm Dockerfile"
                        void $ exec $ "sudo docker run -w /srv -p 8000:8000 -i -t -v $PWD/docker/data:/var/lib/postgresql -v $PWD/snaplets:/srv/snaplets -v $PWD/static:/srv/static -v $PWD/src:/srv/src -v $PWD/devel.cfg:/srv/devel.cfg -v $PWD/defaults.cfg:/srv/defaults.cfg " ++ proj ++ "_devel"
     "test" ~> void (exec $ "cabal exec -- runghc -isrc spec/Main.hs -m \"" ++ (intercalate " " (tail targets) ++ "\""))
     "db" ~> do pass <- liftIO $ require conf (T.pack "database-password")
                let c = "PGPASSWORD=" ++ pass ++ " psql " ++ proj
                        ++ "_devel -U" ++ proj ++ "_user" ++ " -hlocalhost"
                void $ exec c
     "db:create" ~> do pass <- liftIO $ require conf (T.pack "database-password")
                       code <- exec $ "PGPASSWORD=" ++ pass ++ " psql -U" ++ proj ++ "_user template1 -c 'SELECT 1'"
                       isSuper <- case code of
                                    ExitFailure _ -> do void $ exec $ "sudo -u postgres psql template1 -c \"CREATE USER " ++ proj ++ "_user WITH SUPERUSER PASSWORD '" ++ pass ++ "'\""
                                                        return True
                                    ExitSuccess -> do res <- readExec $ "psql -U" ++ proj ++ "_user template1 -c \"SELECT current_setting('is_superuser')\""
                                                      return ("on" `isInfixOf` res)

                       if isSuper
                          then do exec $ "PGPASSWORD=" ++ pass ++ " psql -U" ++ proj ++ "_user template1 -c \"CREATE DATABASE " ++ proj ++ "_devel\""
                                  exec $ "PGPASSWORD=" ++ pass ++ " psql -U" ++ proj ++ "_user template1 -c \"CREATE DATABASE " ++ proj ++ "_test\""
                                  return ()
                          else do void $ exec $ "sudo -u postgres psql template1 -c \"CREATE DATABASE " ++ proj ++ "_devel\""
                                  void $ exec $ "sudo -u postgres psql template1 -c \"CREATE DATABASE " ++ proj ++ "_test\""
                                  void $ exec $ "sudo -u postgres psql template1 -c \"GRANT ALL ON DATABASE " ++ proj ++ "_devel TO " ++ proj ++ "_user\""
                                  void $ exec $ "sudo -u postgres psql template1 -c \"GRANT ALL ON DATABASE " ++ proj ++ "_test TO " ++ proj ++ "_user\""
     "db:new" ~> do liftIO $ putStrLn "Migration name (no spaces, lowercase): "
                    name <- liftIO getLine
                    now <- liftIO getCurrentTime
                    let str = (formatTime defaultTimeLocale "%Y%m%d%H%M%S_" now) ++ name ++ ".hs"
                    liftIO $ putStrLn $ "Writing to migrations/" ++ str ++ "..."
                    liftIO $ writeFile ("migrations/" ++ str) migrationTemplate
     "deps/dbp/migrate.d/.cabal-sandbox/bin/migrate" *> \_ ->
          do need ["deps/dbp/migrate.d/.rivetclone"]
             () <- cmd (Cwd "deps/dbp/migrate.d") "cabal sandbox init"
             cmd (Cwd "deps/dbp/migrate.d") "cabal install"
     "db:migrate" ~> do need ["deps/dbp/migrate.d/.cabal-sandbox/bin/migrate"]
                        void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate up devel"
                        void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate up test"
     "db:migrate:down" ~> do need ["deps/dbp/migrate.d/.cabal-sandbox/bin/migrate"]
                             void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate down devel"
                             void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate down test"
     "db:status" ~> do need ["deps/dbp/migrate.d"]
                       need ["deps/dbp/migrate.d/.cabal-sandbox/bin/migrate"]
                       void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate status devel"
                       void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate status test"
     "db:migrate:docker" ~>
       do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
          exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
          exec "rm Dockerfile"
          void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=up\" -v $PWD/docker/data:/var/lib/postgresql "
                        ++ proj ++ "_migrate"
     "db:migrate:down:docker" ~>
       do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
          exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
          exec "rm Dockerfile"
          void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=down\" -v $PWD/docker/data:/var/lib/postgresql "
                         ++ proj ++ "_migrate"
     "db:status:docker" ~>
       do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
          exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
          exec "rm Dockerfile"
          void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=status\" -v $PWD/docker/data:/var/lib/postgresql "
                        ++ proj ++ "_migrate"
     "repl" ~> void (exec "cabal repl")
     "cabal.sandbox.config" *> \_ -> cmd "cabal sandbox init"
     "deps" ~> need depDirs
     "setup" ~> do need ["cabal.sandbox.config"]
                   need ["deps"]
                   need ["update"]
                   exec "cabal exec -- ghc-pkg expose hspec2"
                   exec "cabal exec -- ghc-pkg expose hspec-snap"
                   void $ exec "cabal exec -- ghc-pkg hide resource-pool"
     "update" ~> void (exec "cabal install -fdevelopment --only-dependencies --enable-tests --reorder-goals --force-reinstalls")
     "deploy:migrate" ~> do stageHost <- liftIO $ require conf (T.pack "stage-host")
                            prodImage <- liftIO $ require conf (T.pack "production-image")
                            tag <- getDockerTag proj stageHost "stage"
                            if length tag < 5
                               then liftIO $ putStrLn "Couldn't get tag from staging."
                               else do let c = "sudo docker run -w /srv -i -t -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432 -v /srv/prod.cfg:/srv/prod.cfg " ++ prodImage ++ ":" ++ tag ++ " migrate up prod"
                                       void $ exec $ "ssh " ++ stageHost ++ " " ++ c

     "deploy:status" ~>
       do stageHost <- liftIO $ require conf (T.pack "stage-host")
          prodHost <- liftIO $ require conf (T.pack "prod-host")
          stageTag <- getDockerTag proj stageHost "stage"
          prodTag <- getDockerTag proj prodHost "prod"
          if length stageTag < 5 || length prodTag < 5
             then liftIO $ putStrLn "Couldn't get tags."
             else do liftIO $ putStrLn "Staging is running..."
                     exec $ "git rev-list --format=%B --max-count=1 " ++ stageTag
                     liftIO $ putStrLn "Production is running..."
                     void $ exec $ "git rev-list --format=%B --max-count=1 " ++ prodTag
     "deploy:rollout" ~> do stageHost <- liftIO $ require conf (T.pack "stage-host")
                            prodHost <- liftIO $ require conf (T.pack "prod-host")
                            prodImage <- liftIO $ require conf (T.pack "production-image")
                            tag <- getDockerTag proj stageHost "stage"
                            if length tag < 5
                               then liftIO $ putStrLn "Couldn't get tag from staging."
                               else do liftIO $ putStrLn "Deploying..."
                                       exec $ "git rev-list --format=%B --max-count=1 " ++ tag
                                       void $ exec $ "ssh " ++ prodHost ++ " /srv/deploy.sh " ++ proj ++ " " ++ proj ++ "prod " ++ prodImage ++ " " ++ tag ++ " 1"


migrationTemplate :: String
migrationTemplate = unlines ["{-# LANGUAGE OverloadedStrings #-}"
                            ,""
                            ,"import Control.Monad"
                            ,"import Database.Migrate"
                            ,"import Site"
                            ,""
                            ,"main = runMainSnap app $ do"
                            ,"  upSql runUp"
                            ,"  downSql runDown"
                            ,""
                            ,"runUp = \"\""
                            ,""
                            ,"runDown = \"\""
                            ,""]
