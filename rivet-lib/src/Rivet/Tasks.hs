{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rivet.Tasks where


import           Control.Applicative        ((<$>))
import           Control.Arrow
import           Control.Monad              (filterM, void, when)
import           Data.Char                  (isSpace)
import           Data.Char
import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict        as M
import           Data.List                  (intercalate, isInfixOf, isSuffixOf,
                                             sort)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.Time.Format
import           Database.PostgreSQL.Simple
import           Development.Shake          hiding (getDirectoryContents)
import           Prelude                    hiding ((++))
import           System.Console.GetOpt
import           System.Directory           (copyFile, createDirectory,
                                             createDirectoryIfMissing,
                                             getCurrentDirectory,
                                             getDirectoryContents,
                                             getTemporaryDirectory)
import           System.Environment         (lookupEnv)
import           System.Exit
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

import           Rivet.Common
import           Rivet.TH

-- NOTE(dbp 2014-09-27): These calls load in files from disk using TH.
loadProjectTemplate
loadFile "migrationTemplate" "template/migration.hs"
loadModelTemplate

init projName = liftIO $ do mapM createDirectory (fst tDirTemplate)
                            mapM_ write (snd tDirTemplate)
  where write (f,c) = if isSuffixOf "project.cabal" f
                      then writeFile (projName ++ ".cabal") (insertProjName c)
                      else writeFile f c
        isNameChar c = isAlphaNum c || c == '-'
        insertProjName c = replace "project" (filter isNameChar projName) c

replace old new s = T.unpack . T.replace (T.pack old) (T.pack new) $ T.pack s

-- NOTE(dbp 2014-09-18): Tasks follow
run proj =
  do let binary = "./.cabal-sandbox/bin/" ++ proj
     need [binary]
     void $ exec binary

runDocker proj =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -p 8000:8000 -i -t -v $PWD/docker/data:/var/lib/postgresql -v $PWD/snaplets:/srv/snaplets -v $PWD/static:/srv/static -v $PWD/src:/srv/src -v $PWD/devel.cfg:/srv/devel.cfg -v $PWD/defaults.cfg:/srv/defaults.cfg " ++ proj ++ "_devel"

db proj conf = do pass <- liftIO $ require conf (T.pack "database-password")
                  let c = "PGPASSWORD=" ++ pass ++ " psql -hlocalhost " ++ proj
                          ++ "_devel -U" ++ proj ++ "_user"
                  void $ exec c

dbTest proj conf = do pass <- liftIO $ require conf (T.pack "database-password")
                      let c = "PGPASSWORD=" ++ pass ++ " psql " ++ proj
                              ++ "_test -U" ++ proj ++ "_user" ++ " -hlocalhost"
                      void $ exec c

test targets =
  void (exec $ "cabal exec -- runghc -isrc spec/Main.hs -m \"" ++ (intercalate " " (tail targets) ++ "\""))

dbCreate proj conf =
  do pass <- liftIO $ require conf (T.pack "database-password")
     code <- exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ proj ++ "_user template1 -c 'SELECT 1'"
     isSuper <- case code of
                  ExitFailure _ -> do void $ exec $ "sudo -u postgres psql template1 -c \"CREATE USER " ++ proj ++ "_user WITH SUPERUSER PASSWORD '" ++ pass ++ "'\""
                                      return True
                  ExitSuccess -> do res <- readExec $ "psql -hlocalhost -U" ++ proj ++ "_user template1 -c \"SELECT current_setting('is_superuser')\""
                                    return ("on" `isInfixOf` res)
     if isSuper
        then do exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ proj ++ "_user template1 -c \"CREATE DATABASE " ++ proj ++ "_devel\""
                exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ proj ++ "_user template1 -c \"CREATE DATABASE " ++ proj ++ "_test\""
                return ()
        else do void $ exec $ "sudo -u postgres psql template1 -c \"CREATE DATABASE " ++ proj ++ "_devel\""
                void $ exec $ "sudo -u postgres psql template1 -c \"CREATE DATABASE " ++ proj ++ "_test\""
                void $ exec $ "sudo -u postgres psql template1 -c \"GRANT ALL ON DATABASE " ++ proj ++ "_devel TO " ++ proj ++ "_user\""
                void $ exec $ "sudo -u postgres psql template1 -c \"GRANT ALL ON DATABASE " ++ proj ++ "_test TO " ++ proj ++ "_user\""

dbNew targets =
  do let name = head (tail targets)
     now <- liftIO getCurrentTime
     let modname = (formatTime defaultTimeLocale "M%Y%m%d%H%M%S_" now) ++ name
         str = modname ++ ".hs"
     liftIO $ putStrLn $ "Writing to migrations/" ++ str ++ "..."
     liftIO $ writeFile ("migrations/" ++ str)
                        (replace "MIGRATION_MODULE" modname migrationTemplate)

dbMigrate proj conf =
  do liftIO $ migrate proj conf "devel"
     liftIO $ migrate proj conf "test"

migrate proj conf env =
  do dbuser <- lookupDefault (proj ++ "_user") conf "database-user"
     dbpass <- require conf "database-password"
     dbhost <- lookupDefault "127.0.0.1" conf "database-host"
     dbport <- lookupDefault 5432 conf "database-port"
     c <- connect (ConnectInfo dbhost dbport dbuser dbpass (proj ++ "_" ++ env))
     execute_ c "CREATE TABLE IF NOT EXISTS migrations (name text NOT NULL PRIMARY KEY, run_at timestamptz NOT NULL DEFAULT now())"

     migrations <- sort . map stripSuffix . filter isCode <$> getDirectoryContents "migrations"
     missing <- filterM (notExists c) migrations
     if null missing
        then putStrLn "No migrations to run."
        else do tmp <- getTemporaryDirectory
                now <- getCurrentTime
                let main = tmp ++ "/migrate_" ++ formatTime defaultTimeLocale "%Y%m%d%H%M%S_" now ++ env ++ ".hs"
                putStrLn $ "Writing migration script to " ++ main ++ "..."
                writeFile main $ "import Database.PostgreSQL.Simple\nimport Rivet.Migration\n" ++
                                 (unlines $ map createImport missing) ++
                                 "\nmain = do\n" ++
                                 (formatconnect dbhost dbport dbuser dbpass (proj ++ "_" ++ env)) ++
                                 (unlines $ map createRun missing)
                -- exec $ "cabal exec -- runghc -isrc -imigrations " ++ main
  where stripSuffix = reverse . drop 3 . reverse
        isCode = isSuffixOf ".hs"
        notExists c m =
          null <$> liftIO (getMigration c m)
        getMigration :: Connection -> String -> IO [(Only String)]
        getMigration c m = query c "SELECT name FROM migrations WHERE name = ?" (Only m)
        createImport m = "import qualified " ++ m
        createRun m = "  run c Up " ++ m ++ ".migrate"
        formatconnect h p u ps nm = "  c <- connect (ConnectInfo " ++ w h ++ " " ++ show p ++ " " ++ w u ++ " " ++ w ps ++ " " ++ w nm ++ ")\n"
          where w s = "\"" ++ s ++ "\""

dbMigrateDown =
  do need ["deps/dbp/migrate.d/.cabal-sandbox/bin/migrate"]
     void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate down devel"
     void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate down test"

dbStatus = do need ["deps/dbp/migrate.d/.cabal-sandbox/bin/migrate"]
              void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate status devel"
              void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate status test"

dbMigrateDocker proj =
  do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=up\" -v $PWD/docker/data:/var/lib/postgresql "
                   ++ proj ++ "_migrate"

dbMigrateDownDocker proj =
  do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=down\" -v $PWD/docker/data:/var/lib/postgresql "
                    ++ proj ++ "_migrate"

dbStatusDocker proj =
  do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=status\" -v $PWD/docker/data:/var/lib/postgresql "
                   ++ proj ++ "_migrate"

modelNew proj (_:nm:fields) =
  do exec $ "mkdir -p src/" ++ nm
     liftIO $ do mapM (createDirectory . addPath) (fst tModelTemplate)
                 mapM_ (uncurry writeFile . (addPath *** (replace nm))) (snd tModelTemplate)
  where addPath = (("src" </> nm) </>)
        replace nm c = T.unpack .
                       T.replace "mODEL" (T.toLower nm') .
                       T.replace "MODEL" nm' .
                       T.pack $ c
          where nm' = T.pack nm


repl = void (exec "cabal repl")

setup = do need ["cabal.sandbox.config"]
           need ["deps"]
           exec "cabal install -fdevelopment --only-dependencies --enable-tests --reorder-goals --force-reinstalls"
           exec "cabal exec -- ghc-pkg expose hspec2"
           exec "cabal exec -- ghc-pkg expose hspec-snap"
           void $ exec "cabal exec -- ghc-pkg hide resource-pool"

cryptEdit proj =
  do e <- doesFileExist ".rivetcrypt"
     let decrypted = "/tmp/rivetdecrypted-" ++ proj
     editor <- fromMaybe "vi" <$> liftIO (lookupEnv "EDITOR")
     if e
        then exec $ "openssl enc -aes-256-cbc -d -a -salt -in .rivetcrypt -out " ++ decrypted ++ " -pass file:.rivetpass"
        else exec $ "touch " ++ decrypted
     exec $ editor ++ " " ++ decrypted
     exec $ "openssl enc -aes-256-cbc -e -a -salt -in " ++ decrypted ++ " -out .rivetcrypt -pass file:.rivetpass"
     void $ exec $ "rm " ++ decrypted

cryptShow =
  do e <- doesFileExist ".rivetcrypt"
     if e
        then void $ exec $ "openssl enc -aes-256-cbc -d -a -salt -in .rivetcrypt -pass file:.rivetpass"
        else liftIO $ putStrLn "No .rivetcrypt."

cryptSetPass proj =
  do e <- doesFileExist ".rivetcrypt"
     let decrypted = "/tmp/rivetdecrypted-" ++ proj
     liftIO $ putStrLn "Enter new passphrase (will be stored in .rivetpass):"
     pass <- liftIO getLine
     if e
        then exec $ "openssl enc -aes-256-cbc -d -a -salt -in .rivetcrypt -out " ++ decrypted ++ " -pass file:.rivetpass"
        else exec $ "touch " ++ decrypted
     liftIO $ copyFile ".rivetpass" ".rivetpass-0"
     liftIO $ writeFile ".rivetpass" pass
     exec $ "openssl enc -aes-256-cbc -e -a -salt -in " ++ decrypted ++ " -out .rivetcrypt -pass file:.rivetpass"
     void $ exec $ "rm " ++ decrypted
