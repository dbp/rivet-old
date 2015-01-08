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
import           Data.List                  (intercalate, intersperse,
                                             isInfixOf, isSuffixOf, sort)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.Time.Format
import           Database.PostgreSQL.Simple
import           Development.Shake          hiding (createDirectory',
                                             doesDirectoryExist,
                                             getDirectoryContents, writeFile')
import           Prelude                    hiding ((++))
import           System.Console.GetOpt
import           System.Directory           (copyFile, createDirectory,
                                             createDirectoryIfMissing,
                                             doesDirectoryExist,
                                             getCurrentDirectory,
                                             getDirectoryContents,
                                             getTemporaryDirectory, removeFile)
import           System.Environment         (lookupEnv)
import           System.Exit
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Locale              (defaultTimeLocale)
import           System.Process

import           Rivet.Common
import           Rivet.TH

createDirectory' d = do putStrLn $ "creating " ++ d
                        createDirectory d
writeFile' f c = do putStrLn $ "writing " ++ f
                    writeFile f c

-- NOTE(dbp 2014-09-27): These calls load in files from disk using TH.
loadProjectTemplate
loadFile "migrationTemplate" "template/migration.hs"

init projName = do liftIO $ do mapM createDirectory' (fst tDirTemplate)
                               mapM_ write (snd tDirTemplate)
                   hasGit <- liftIO $ doesDirectoryExist ".git"
                   if hasGit
                      then liftIO $ putStrLn "detected existing .git directory, not committing."
                      else do void $ exec "git init"
                              void $ exec "git add ."
                              void $ exec "git commit -m 'initial commit'"
  where write (f,c) =
          if isSuffixOf "project.cabal" f
          then writeFile' (projName ++ ".cabal") (insertProjName c)
          else writeFile' f (replace "PROJECT" (dbIfy projName) c)
        isNameChar c = isAlphaNum c || c == '-'
        insertProjName c = replace "project" (filter isNameChar projName) c

replace old new s = T.unpack . T.replace (T.pack old) (T.pack new) $ T.pack s

-- NOTE(dbp 2014-09-18): Tasks follow
run proj =
  do let binary = "./.cabal-sandbox/bin/" ++ proj
     need [binary]
     void $ exec binary

dbIfy = T.unpack . T.replace "-" "_" . T.pack

db proj conf = do pass <- liftIO $ require conf (T.pack "database-password")
                  port <- liftIO $ lookupDefault 5432 conf (T.pack "database-port") :: Action Int
                  user <- liftIO $ lookupDefault (dbIfy proj ++ "_user") conf (T.pack "database-user")
                  let c = "PGPASSWORD=" ++ pass ++ " psql -hlocalhost " ++ dbIfy proj
                          ++ "_devel -U" ++ user ++ " -p " ++ show port
                  void $ exec c

dbTest proj conf =
  do pass <- liftIO $ require conf (T.pack "database-password")
     port <- liftIO $ lookupDefault 5432 conf (T.pack "database-port") :: Action Int
     user <- liftIO $ lookupDefault (dbIfy proj ++ "_user") conf (T.pack "database-user")
     let c = "PGPASSWORD=" ++ pass ++ " psql " ++ dbIfy proj
             ++ "_test -U" ++ user ++ " -hlocalhost" ++ " -p " ++ show port
     void $ exec c

test cabal targets =
  do code <- exec $ cabal ++ " exec -- runghc -isrc -ispec spec/Main.hs -m \"" ++ (intercalate " " (tail targets) ++ "\"")
     case code of
       ExitSuccess -> return ()
       _ -> error "rivet test: Test Failure."

dbCreate proj conf =
  do pass <- liftIO $ require conf (T.pack "database-password")
     user <- liftIO $ lookupDefault (dbIfy proj ++ "_user") conf (T.pack "database-user")
     let dbname = dbIfy proj
     code <- exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ user ++ " template1 -c 'SELECT 1'"
     isSuper <- case code of
                  ExitFailure _ -> do void $ exec $ "psql template1 -c \"CREATE USER " ++ user ++ " WITH SUPERUSER PASSWORD '" ++ pass ++ "'\""
                                      return True
                  ExitSuccess -> do res <- readExec $ "psql -hlocalhost -U" ++ user ++ " template1 -c \"SELECT current_setting('is_superuser')\""
                                    return ("on" `isInfixOf` res)
     if isSuper
        then do exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ user ++ " template1 -c \"CREATE DATABASE " ++ dbname ++ "_devel\""
                exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ user ++ " template1 -c \"CREATE DATABASE " ++ dbname ++ "_test\""
                return ()
        else do void $ exec $ "psql template1 -c \"CREATE DATABASE " ++ dbname ++ "_devel\""
                void $ exec $ "psql template1 -c \"CREATE DATABASE " ++ dbname ++ "_test\""
                void $ exec $ "psql template1 -c \"GRANT ALL ON DATABASE " ++ dbname ++ "_devel TO " ++ user ++ "\""
                void $ exec $ "psql template1 -c \"GRANT ALL ON DATABASE " ++ dbname ++ "_test TO " ++ user ++ "\""

dbNew targets =
  do let name = head (tail targets)
     liftIO $ genMigration name sqlud
  where sqlud = "sql up down\n\n\
                \up = \"\"\n\
                 \\n\
                 \down = \"\""

genMigration name content =
  do now <- getCurrentTime
     let modname = (formatTime defaultTimeLocale "M%Y%m%d%H%M%S_" now) ++ name
         str = modname ++ ".hs"
     putStrLn $ "Writing to migrations/" ++ str ++ "..."
     writeFile ("migrations/" ++ str)
         (replace "MIGRATION_MODULE" modname . replace "CONTENT" content $ migrationTemplate)

data MigrateMode = Up | Down | Status deriving Show

dbMigrate cabal proj conf [] =
  do liftIO $ migrate cabal proj conf "devel" Up
     liftIO $ migrate cabal proj conf "test" Up
dbMigrate cabal proj conf (env:_) = liftIO $ migrate cabal proj conf env Up

dbMigrateDown cabal proj conf [] =
  do liftIO $ migrate cabal proj conf "devel" Down
     liftIO $ migrate cabal proj conf "test" Down
dbMigrateDown cabal proj conf (env:_) = liftIO $ migrate cabal proj conf env Down

dbStatus cabal proj conf [] = do liftIO $ migrate cabal proj conf "devel" Status
                                 liftIO $ migrate cabal proj conf "test" Status
dbStatus cabal proj conf (env:_) = liftIO $ migrate cabal proj conf env Status

migrate cabal proj conf env mode =
  do dbuser <- lookupDefault (dbIfy proj ++ "_user") conf "database-user"
     dbpass <- require conf "database-password"
     dbhost <- lookupDefault "127.0.0.1" conf "database-host"
     dbport <- lookupDefault 5432 conf "database-port"
     dbname <- lookupDefault (dbIfy proj ++ "_" ++ env) conf "database-name"
     c <- connect (ConnectInfo dbhost dbport dbuser dbpass dbname)
     execute_ c "CREATE TABLE IF NOT EXISTS migrations (name text NOT NULL PRIMARY KEY, run_at timestamptz NOT NULL DEFAULT now())"
     tmp <- getTemporaryDirectory
     now <- getCurrentTime
     let main = tmp ++ "/migrate_" ++ formatTime defaultTimeLocale "%Y%m%d%H%M%S_" now ++ env ++ ".hs"
     migrations <- sort . map stripSuffix . filter isCode <$>
                   getDirectoryContents "migrations"
     run <- case mode of
              Up ->
                do missing <- filterM (notExists c) migrations
                   if null missing
                      then putStrLn "No migrations to run." >> return False
                      else do putStrLn $ "Writing migration script to " ++ main ++ "..."
                              writeFile main $
                                "import Database.PostgreSQL.Simple\nimport Rivet.Migration\n" ++
                                (unlines $ map createImport missing) ++
                                "\nmain = do\n" ++
                                (formatconnect dbhost dbport dbuser dbpass dbname) ++
                                (unlines $ map (createRun mode) missing)
                              return True
              Down -> do toDown <- dropWhileM (notExists c) $ reverse migrations
                         case toDown of
                           (x:_) -> do putStrLn $ "Writing migration script to " ++ main ++ "..."
                                       writeFile main $
                                         "import Database.PostgreSQL.Simple\nimport Rivet.Migration\n" ++
                                         createImport x ++
                                         "\nmain = do\n" ++
                                         (formatconnect dbhost dbport dbuser dbpass dbname) ++
                                         createRun mode x
                                       return True
                           _ -> putStrLn "No migrations remaining." >> return False
              Status -> do mapM_ (\m -> do ne <- notExists c m
                                           if ne
                                              then putStrLn $ m ++ " in " ++ env
                                              else putStrLn $ " APPLIED " ++ m ++ " in " ++ env)
                                 migrations
                           return False
     when run $ do putStrLn $ "Running " ++ main ++ "..."
                   system $ cabal ++ " exec -- runghc -isrc -imigrations " ++ main
                   putStrLn $ "Cleaning up... "
                   removeFile main
  where stripSuffix = reverse . drop 3 . reverse
        isCode = isSuffixOf ".hs"
        dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
        dropWhileM f [] = return []
        dropWhileM f (x:xs) = do r <- f x
                                 if r
                                    then dropWhileM f xs
                                    else return (x:xs)
        notExists c m =
          null <$> liftIO (getMigration c m)
        getMigration :: Connection -> String -> IO [(Only String)]
        getMigration c m = query c "SELECT name FROM migrations WHERE name = ?" (Only m)
        createImport m = "import qualified " ++ m
        createRun mode m = "  run " ++ w m ++ " c " ++ show mode ++ " " ++ m ++ ".migrate >> putStrLn \"Ran " ++ m ++ "\""
        formatconnect h p u ps nm = "  c <- connect (ConnectInfo " ++ w h ++ " " ++ show p ++ " " ++ w u ++ " " ++ w ps ++ " " ++ w nm ++ ")\n"
        w s = "\"" ++ s ++ "\""



repl cabal = void (exec $ cabal ++ " repl")

setup cabal = do need ["cabal.sandbox.config"]
                 need ["deps"]
                 exec $ cabal ++ " install -fdevelopment --only-dependencies --enable-tests --reorder-goals --force-reinstalls"
                 exec $ cabal ++ " exec -- ghc-pkg expose hspec"
                 exec $ cabal ++ " exec -- ghc-pkg expose hspec-snap"
                 void $ exec $ cabal ++ " exec -- ghc-pkg hide resource-pool"

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
