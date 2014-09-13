module Main where

import           Control.Applicative ((<$>))
import           Control.Monad       (void)
import           Data.Configurator
import           Data.Monoid
import qualified Data.Text           as T
import           Development.Shake
import           Prelude             hiding ((++))
import           System.Directory
import           System.Process


(++) :: Monoid a => a -> a -> a
(++) = mappend

opts :: ShakeOptions
opts = shakeOptions { shakeFiles    = ".shake/" }

getProjectName :: IO String
getProjectName = (reverse . takeWhile (/= '/') . reverse) <$>
                   getCurrentDirectory

exec :: String -> Action ()
exec c = liftIO $ do putStrLn c
                     void $ system c


main :: IO ()
main = do
  proj <- getProjectName
  conf <- load [Optional "Rivetfile"]
  deps <- lookupDefault [] conf (T.pack "dependencies")
  let depDirs = map ((++ ".d") . ("deps/" ++) . T.unpack . head .
                     T.splitOn (T.pack ":") . head . T.splitOn (T.pack "+")) deps
  shakeArgs opts $ do
     sequence_ (map (\d ->
       do let (repo':rest) = T.splitOn (T.pack "+") d
          let (repo:branchspec) = T.splitOn (T.pack ":") repo'
          ("deps/" ++ (T.unpack repo) ++ ".d") *> \depdir ->
            do cmd ("git clone https://github.com/"
                    ++ (T.unpack repo) ++ " " ++ depdir) :: Action ()
               case branchspec of
                 (branch:_) -> cmd (Cwd depdir) ("git checkout " ++ (T.unpack branch))
                 _ -> return ()
               case rest of
                 (subdirs:_) -> mapM_ (\subdir -> cmd ("cabal sandbox add-source " ++ depdir
                                                        ++ "/" ++ (T.unpack subdir)) :: Action ())
                                      (T.splitOn (T.pack ",") subdirs)
                 _ -> cmd ("cabal sandbox add-source " ++ depdir))
        deps)
     let binary = "./.cabal-sandbox/bin/" ++ proj
     binary *> \_ -> do files <- getDirectoryFiles "" ["src//*.hs", "*.cabal"]
                        liftIO $ print files
                        need files
                        cmd "cabal install -fdevelopment --reorder-goals"
     "run" ~> do need [binary]
                 exec binary
     "test" ~> exec "cabal exec -- runghc -isrc spec/Main.hs"
     "db" ~> do pass <- liftIO $ require conf (T.pack "database-password")
                let c = "PGPASSWORD=" ++ pass ++ " psql " ++ proj
                        ++ "_devel -U" ++ proj ++ "_user" ++ " -hlocalhost"
                exec c
     "repl" ~> exec "cabal repl"
     "cabal.sandbox.config" *> \_ -> cmd "cabal sandbox init"
     "init" ~> do need ["cabal.sandbox.config"]
                  liftIO $ createDirectoryIfMissing False "deps"
                  need depDirs
