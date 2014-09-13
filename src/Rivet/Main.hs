module Main where

import           Control.Applicative ((<$>))
import           Control.Monad       (void)
import           Data.Configurator
import qualified Data.Text           as T
import           Development.Shake
import           System.Directory
import           System.Process

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
  shakeArgs opts $ do
     let binary = "./.cabal-sandbox/bin/" ++ proj
     "test" ~> exec "cabal exec -- runghc -isrc spec/Main.hs"
     binary *> \_ -> do files <- getDirectoryFiles "" ["src//*.hs", "*.cabal"]
                        liftIO $ print files
                        need files
                        cmd "cabal install -fdevelopment --reorder-goals"
     "run" ~> do need [binary]
                 exec binary
     "db" ~> do pass <- liftIO $ require conf (T.pack "database-password")
                let c = "PGPASSWORD=" ++ pass ++ " psql " ++ proj ++ "_devel -U" ++ proj ++ "_user" ++ " -hlocalhost"
                exec c
