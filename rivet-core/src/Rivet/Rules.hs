module Rivet.Rules where

import           Prelude           hiding ((++))

import           Control.Monad     (void, when)
import           Data.List         (isInfixOf)
import qualified Data.Text         as T
import           Development.Shake

import           Rivet.Common

addCommands commands =
  sequence_ (map (\(cName, cCom) -> (T.unpack cName) ~> void (exec (T.unpack cCom)))
                 commands)

addBinary cabal proj =
  do let binary = ".cabal-sandbox/bin/" ++ proj
     binary *> \_ -> do files <- getDirectoryFiles "" ["src/Main.hs", "*.cabal"]
                        need files
                        cmd $ cabal ++ " install -j -fdevelopment --reorder-goals --force-reinstalls"

addDependencies cabal deps =
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
            let addSource s = do contents <- readFileOrBlank "deps/add-all"
                                 let addstr = cabal ++ " sandbox add-source " ++ s
                                 if addstr `isInfixOf` contents
                                    then return ()
                                    else liftIO $ appendFile "deps/add-all" (addstr ++ "\n")
                                 let remstr = cabal ++ " sandbox delete-source " ++ s
                                 if remstr `isInfixOf` contents
                                    then return ()
                                    else liftIO $ appendFile "deps/delete-all" (remstr ++ "\n")
                                 -- NOTE(dbp 2015-01-06): This is an imperfect test, but one that is
                                 -- easy to mock up, if your sandbox is structured differently.
                                 hasSandbox <- doesFileExist "cabal.sandbox.config"
                                 if hasSandbox
                                    then cmd addstr :: Action ()
                                    else return ()
                                 hasHalcyon <- doesDirectoryExist ".halcyon"
                                 when hasHalcyon $
                                   do contents <- readFileOrBlank ".halcyon/sandbox-sources"
                                      if s `isInfixOf` contents
                                         then return ()
                                         else liftIO $ appendFile ".halcyon/sandbox-sources" (s ++ "\n")

            case rest of
              (subdirs:_) -> mapM_ (\subdir -> addSource (depdir ++ "/" ++ (T.unpack subdir)))
                                   (T.splitOn (T.pack ",") subdirs)
              _ -> addSource depdir)
        deps)
     "deps" ~> need depDirs
  where readFileOrBlank nm = do e <- doesFileExist nm
                                if e
                                   then liftIO $ readFile nm
                                   else return ""
