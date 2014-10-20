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

-- NOTE(dbp 2014-09-27): These calls load in files from disk using TH.
loadProjectTemplate
loadFile "migrationTemplate" "template/migration.hs"
loadModelTemplate
loadFile "modelNewHeist" "template/heist/new.tpl"
loadFile "modelEditHeist" "template/heist/edit.tpl"
loadFile "modelFormHeist" "template/heist/_form.tpl"

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

data MigrateMode = Up | Down | Status deriving Show

dbMigrate proj conf =
  do liftIO $ migrate proj conf "devel" Up
     liftIO $ migrate proj conf "test" Up

dbMigrateDown proj conf =
  do liftIO $ migrate proj conf "devel" Down
     liftIO $ migrate proj conf "test" Down

dbStatus proj conf = do liftIO $ migrate proj conf "devel" Status
                        liftIO $ migrate proj conf "test" Status

migrate proj conf env mode =
  do dbuser <- lookupDefault (proj ++ "_user") conf "database-user"
     dbpass <- require conf "database-password"
     dbhost <- lookupDefault "127.0.0.1" conf "database-host"
     dbport <- lookupDefault 5432 conf "database-port"
     c <- connect (ConnectInfo dbhost dbport dbuser dbpass (proj ++ "_" ++ env))
     execute_ c "CREATE TABLE IF NOT EXISTS migrations (name text NOT NULL PRIMARY KEY, run_at timestamptz NOT NULL DEFAULT now())"
     tmp <- getTemporaryDirectory
     now <- getCurrentTime
     let main = tmp ++ "/migrate_" ++ formatTime defaultTimeLocale "%Y%m%d%H%M%S_" now ++ env ++ ".hs"
     putStrLn $ "Writing migration script to " ++ main ++ "..."
     migrations <- sort . map stripSuffix . filter isCode <$>
                   getDirectoryContents "migrations"
     case mode of
       Up ->
         do missing <- filterM (notExists c) migrations
            if null missing
               then putStrLn "No migrations to run."
               else writeFile main $
                      "import Database.PostgreSQL.Simple\nimport Rivet.Migration\n" ++
                      (unlines $ map createImport missing) ++
                      "\nmain = do\n" ++
                      (formatconnect dbhost dbport dbuser dbpass (proj ++ "_" ++ env)) ++
                      (unlines $ map (createRun mode) missing)
       Down -> do toDown <- dropWhileM (notExists c) $ reverse migrations
                  case toDown of
                    (x:_) -> writeFile main $
                               "import Database.PostgreSQL.Simple\nimport Rivet.Migration\n" ++
                               createImport x ++
                               "\nmain = do\n" ++
                               (formatconnect dbhost dbport dbuser dbpass (proj ++ "_" ++ env)) ++
                               createRun mode x
                    _ -> putStrLn "No migrations remaining."
       Status -> mapM_ (\m -> do ne <- notExists c m
                                 if ne
                                    then putStrLn $ m ++ " in " ++ env
                                    else putStrLn $ " APPLIED " ++ m ++ " in " ++ env)
                       migrations
     case mode of
       Status -> return ()
       _ -> do putStrLn $ "Running " ++ main ++ "..."
               system $ "cabal exec -- runghc -isrc -imigrations " ++ main
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
        createRun mode m = "  run " ++ w m ++ " c " ++ show mode ++ " " ++ m ++ ".migrate"
        formatconnect h p u ps nm = "  c <- connect (ConnectInfo " ++ w h ++ " " ++ show p ++ " " ++ w u ++ " " ++ w ps ++ " " ++ w nm ++ ")\n"
        w s = "\"" ++ s ++ "\""


modelNew proj (_:nm:fields') =
  do exec $ "mkdir -p src/" ++ nm
     liftIO $ do mapM (createDirectory . addPath) (fst tModelTemplate)
                 mapM_ (uncurry writeFile . (addPath *** (replace nm))) (snd tModelTemplate)
     let lnm = map toLower nm
     exec $ "mkdir -p templates/" ++ lnm
     liftIO $ writeFile ("templates" </> lnm </> "new.tpl") (replace nm modelNewHeist)
     liftIO $ writeFile ("templates" </> lnm </> "edit.tpl") (replace nm modelEditHeist)
     liftIO $ writeFile ("templates" </> lnm </> "_form.tpl") (replace nm modelFormHeist)
  where addPath = (("src" </> nm) </>)
        fields = map ((\(a:b:[]) -> (a,b)) . T.splitOn ":" . T.pack) fields'
        num = length fields
        mfields = unws $ map (\((nm,_),var) -> ", " ++ nm ++ " :: " ++ T.pack [var])
                             (zip fields ['b'..])
        mtypes_f = unws $ map (\(_,ty) -> "(f " ++ ty ++ ")") fields
        mwires = unws $ map (\(nm,_) -> "(Wire \"" ++ nm ++ "\")") fields
        umfields = unws $ map fst fields
        mjusts = unws $ map (\(nm,_) -> "(Just " ++ nm ++ ")") fields
        mform = T.intercalate " <*> " $ map (\(nm,ty) -> "\"" ++ nm ++ "\" .: " ++ mkform ty) fields
        mkform "Text" = "text Nothing"
        mkform ty = "stringRead \"Must be a(n) " ++ ty ++ "\" Nothing"
        meditform = T.intercalate " <*> " $
          map (\(nm,ty) -> "\"" ++ nm ++ "\" .: " ++ mkeditform nm ty) fields
        mkeditform nm "Text" = "text (Just " ++ nm ++ ")"
        mkeditform nm ty = "stringRead \"Must be a(n) " ++ ty ++ "\" (Just " ++ nm ++ ")"
        msplices = T.intercalate "\n" $
          map (\(nm,ty) -> "     \"" ++ nm ++ "\" ## textSplice " ++ mksplice nm ty) fields
        mksplice nm "Text" = nm
        mksplice nm ty = "$ tshow " ++ nm
        mdffields = T.intercalate "\n\n" $ map (\(nm,ty) -> "  <dfLabel ref=\"" ++ nm ++ "\">" ++ nm ++ "</dfLabel>\n  <dfInputText ref=\"" ++ nm ++ "\"/>") fields
        unws = T.intercalate " "
        replace nm c = T.unpack .
                       T.replace "mODEL" (T.toLower nm') .
                       T.replace "MODEL" nm' .
                       T.replace "MVARS" (T.pack $ intersperse ' ' $ take num ['b'..]) .
                       T.replace "MFIELDS" mfields .
                       T.replace "MTYPES_F" mtypes_f .
                       T.replace "MWIRES" mwires .
                       T.replace "mFIELDS" umfields .
                       T.replace "mJUSTS" mjusts .
                       T.replace "MFORM" mform .
                       T.replace "MEDITFORM" meditform .
                       T.replace "MSPLICES" msplices .
                       T.replace "MDFFIELDS" mdffields .
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
