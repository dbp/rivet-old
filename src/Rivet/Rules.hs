module Rivet.Rules where

import           Control.Monad     (void)
import qualified Data.Text         as T
import           Development.Shake
import           Prelude           hiding ((++))

import           Rivet.Common

addCommands commands =
  sequence_ (map (\(cName, cCom) -> (T.unpack cName) ~> void (exec (T.unpack cCom)))
                 commands)

addBinary proj =
  do let binary = "./.cabal-sandbox/bin/" ++ proj
     binary *> \_ -> do files <- getDirectoryFiles "" ["src/Main.hs", "*.cabal"]
                        need files
                        cmd "cabal install -fdevelopment --reorder-goals --force-reinstalls"

addDependencies deps =
  do let depDirs = map ((++ ".d/.rivetclone") . ("deps/" ++) . T.unpack . head .
                        T.splitOn (T.pack ":") . head . T.splitOn (T.pack "+")) deps
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
     "deps" ~> need depDirs
     "deps/dbp/migrate.d/.cabal-sandbox/bin/migrate" *> \_ ->
          do need ["deps/dbp/migrate.d/.rivetclone"]
             () <- cmd (Cwd "deps/dbp/migrate.d") "cabal sandbox init"
             cmd (Cwd "deps/dbp/migrate.d") "cabal install"
