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
loadModelTemplate
loadFile "modelNewHeist" "template/heist/new.tpl"
loadFile "modelEditHeist" "template/heist/edit.tpl"
loadFile "modelFormHeist" "template/heist/_form.tpl"
loadFile "modelShowHeist" "template/heist/show.tpl"
loadFile "modelIndexHeist" "template/heist/index.tpl"

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

test targets =
  void (exec $ "cabal exec -- runghc -isrc -ispec spec/Main.hs -m \"" ++ (intercalate " " (tail targets) ++ "\""))

dbCreate proj conf =
  do pass <- liftIO $ require conf (T.pack "database-password")
     user <- liftIO $ lookupDefault (dbIfy proj ++ "_user") conf (T.pack "database-user")
     let dbname = dbIfy proj
     code <- exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ user ++ " template1 -c 'SELECT 1'"
     isSuper <- case code of
                  ExitFailure _ -> do void $ exec $ "sudo -u postgres psql template1 -c \"CREATE USER " ++ user ++ " WITH SUPERUSER PASSWORD '" ++ pass ++ "'\""
                                      return True
                  ExitSuccess -> do res <- readExec $ "psql -hlocalhost -U" ++ user ++ " template1 -c \"SELECT current_setting('is_superuser')\""
                                    return ("on" `isInfixOf` res)
     if isSuper
        then do exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ user ++ " template1 -c \"CREATE DATABASE " ++ dbname ++ "_devel\""
                exec $ "PGPASSWORD=" ++ pass ++ " psql -hlocalhost -U" ++ user ++ " template1 -c \"CREATE DATABASE " ++ dbname ++ "_test\""
                return ()
        else do void $ exec $ "sudo -u postgres psql template1 -c \"CREATE DATABASE " ++ dbname ++ "_devel\""
                void $ exec $ "sudo -u postgres psql template1 -c \"CREATE DATABASE " ++ dbname ++ "_test\""
                void $ exec $ "sudo -u postgres psql template1 -c \"GRANT ALL ON DATABASE " ++ dbname ++ "_devel TO " ++ user ++ "\""
                void $ exec $ "sudo -u postgres psql template1 -c \"GRANT ALL ON DATABASE " ++ dbname ++ "_test TO " ++ user ++ "\""

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

dbMigrate proj conf [] =
  do liftIO $ migrate proj conf "devel" Up
     liftIO $ migrate proj conf "test" Up
dbMigrate proj conf (env:_) = liftIO $ migrate proj conf env Up

dbMigrateDown proj conf [] =
  do liftIO $ migrate proj conf "devel" Down
     liftIO $ migrate proj conf "test" Down
dbMigrateDown proj conf (env:_) = liftIO $ migrate proj conf env Down

dbStatus proj conf [] = do liftIO $ migrate proj conf "devel" Status
                           liftIO $ migrate proj conf "test" Status
dbStatus proj conf (env:_) = liftIO $ migrate proj conf env Status

migrate proj conf env mode =
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
        createRun mode m = "  run " ++ w m ++ " c " ++ show mode ++ " " ++ m ++ ".migrate >> putStrLn \"Ran " ++ m ++ "\""
        formatconnect h p u ps nm = "  c <- connect (ConnectInfo " ++ w h ++ " " ++ show p ++ " " ++ w u ++ " " ++ w ps ++ " " ++ w nm ++ ")\n"
        w s = "\"" ++ s ++ "\""


modelNew proj (_:nm:fields') =
  do exec $ "mkdir -p src/" ++ nm
     liftIO $ do mapM (createDirectory' . addPath) (fst tModelTemplate)
                 mapM_ (uncurry writeFile' . (addPath *** (replace nm))) (snd tModelTemplate)
     exec $ "mkdir -p templates/" ++ lnm
     liftIO $ writeFile' ("templates" </> lnm </> "new.tpl") (replace nm modelNewHeist)
     liftIO $ writeFile' ("templates" </> lnm </> "edit.tpl") (replace nm modelEditHeist)
     liftIO $ writeFile' ("templates" </> lnm </> "_form.tpl") (replace nm modelFormHeist)
     liftIO $ writeFile' ("templates" </> lnm </> "show.tpl") (replace nm modelShowHeist)
     liftIO $ writeFile' ("templates" </> lnm </> "index.tpl") (replace nm modelIndexHeist)
     liftIO $ genMigration ("add_" ++ lnm) migr
     liftIO $ putStrLn $ "\nNOTE: While handlers have been created (with top level at " ++ nm ++ ".Handlers.top),\n \
                         \they have not been added to the routes in src/Site.hs. You can put them anywhere,\n \
                         \but some of the templates assume that the top handler will exist at /" ++ lnm ++ "."
  where lnm = map toLower nm
        addPath = (("src" </> nm) </>)
        fields = map ((\(a:b:[]) -> (a,b)) . T.splitOn ":" . T.pack) fields'
        num = length fields
        mfields = unws $ map (\((nm,_),var) -> ", " ++ nm ++ " :: " ++ T.pack [var])
                             (zip fields ['b'..])
        mtypes_f = unws $ map (\(_,ty) -> "(f " ++ ty ++ ")") fields
        mnewfields = unws $ map snd fields
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
                       T.replace "MNEWFIELDS" mnewfields .
                       T.replace "MWIRES" mwires .
                       T.replace "mFIELDS" umfields .
                       T.replace "mJUSTS" mjusts .
                       T.replace "MFORM" mform .
                       T.replace "MEDITFORM" meditform .
                       T.replace "MSPLICES" msplices .
                       T.replace "MDFFIELDS" mdffields .
                       T.pack $ c
          where nm' = T.pack nm
        colspecs = T.intercalate ", " $
                   ("ColumnSpec \"id\" \"serial\" Nothing (Just \"PRIMARY KEY\")") :
                   (map (\(fld, ty) -> "ColumnSpec \"" <> fld <> "\" \"" <>
                                                  toSql ty <> "\" Nothing Nothing")
                       fields)
        toSql :: Text -> Text
        toSql "Text" = "text"
        toSql "Int" = "int"
        toSql "Integer" = "bigint"
        toSql "UTCTime" = "timestamptz"
        toSql t = error $ "I don't know how to deal with columns of type " ++ T.unpack t ++ " yet."
        migr = "createTable \"" ++ lnm ++ "s\" [" ++ T.unpack colspecs ++ "]"


repl = void (exec "cabal repl")

setup = do need ["cabal.sandbox.config"]
           need ["deps"]
           exec "cabal install -fdevelopment --only-dependencies --enable-tests --reorder-goals --force-reinstalls"
           exec "cabal exec -- ghc-pkg expose hspec"
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
