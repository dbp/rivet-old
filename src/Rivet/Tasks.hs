module Rivet.Tasks where

import           Control.Applicative     ((<$>))
import           Control.Monad           (void, when)
import           Data.Char               (isSpace)
import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict     as M
import           Data.List               (intercalate, isInfixOf)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Time.Clock
import           Data.Time.Format
import           Development.Shake
import           Prelude                 hiding ((++))
import           System.Directory        (copyFile, createDirectoryIfMissing,
                                          getCurrentDirectory)
import           System.Environment      (lookupEnv)
import           System.Exit
import           System.IO
import           System.Process

import           Rivet.Common


getDockerTag proj h env = stripWhitespace <$> readExec ("ssh " ++ h ++ " \"docker ps\" | grep " ++ proj ++ "_" ++ env ++ " | awk '{ print $2}' | cut -d ':' -f 2")


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
                  let c = "PGPASSWORD=" ++ pass ++ " psql " ++ proj
                          ++ "_devel -U" ++ proj ++ "_user" ++ " -hlocalhost"
                  void $ exec c

test targets =
  void (exec $ "cabal exec -- runghc -isrc spec/Main.hs -m \"" ++ (intercalate " " (tail targets) ++ "\""))

dbCreate proj conf =
  do pass <- liftIO $ require conf (T.pack "database-password")
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

dbNew targets =
  do let name = head (tail targets)
     now <- liftIO getCurrentTime
     let str = (formatTime defaultTimeLocale "%Y%m%d%H%M%S_" now) ++ name ++ ".hs"
     liftIO $ putStrLn $ "Writing to migrations/" ++ str ++ "..."
     liftIO $ writeFile ("migrations/" ++ str) migrationTemplate

dbMigrate = do need ["deps/dbp/migrate.d/.cabal-sandbox/bin/migrate"]
               void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate up devel"
               void $ exec "./deps/dbp/migrate.d/.cabal-sandbox/bin/migrate up test"
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

repl = void (exec "cabal repl")

setup = do need ["cabal.sandbox.config"]
           need ["deps"]
           exec "cabal install -fdevelopment --only-dependencies --enable-tests --reorder-goals --force-reinstalls"
           exec "cabal exec -- ghc-pkg expose hspec2"
           exec "cabal exec -- ghc-pkg expose hspec-snap"
           void $ exec "cabal exec -- ghc-pkg hide resource-pool"

deployMigrate proj conf =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     tag <- getDockerTag proj stageHost "stage"
     if length tag < 5
        then liftIO $ putStrLn "Couldn't get tag from staging."
        else do let c = "docker run -w /srv -i -t -v /srv/data:/srv/data -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432 -v /srv/prod_" ++ tag ++ ".cfg:/srv/prod.cfg " ++ prodImage ++ ":" ++ tag ++ " migrate up prod"
                void $ exec $ "ssh " ++ stageHost ++ " " ++ c

deployStage proj conf =
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

deployRollout proj conf =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodHost <- liftIO $ require conf (T.pack "prod-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     tag <- getDockerTag proj stageHost "stage"
     if length tag < 5
        then liftIO $ putStrLn "Couldn't get tag from staging."
        else do liftIO $ putStrLn "Deploying..."
                exec $ "git rev-list --format=%B --max-count=1 " ++ tag
                void $ exec $ "ssh " ++ prodHost ++ " /srv/deploy.sh " ++ proj ++ " " ++ proj ++ "prod " ++ prodImage ++ " " ++ tag ++ " 1"


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
