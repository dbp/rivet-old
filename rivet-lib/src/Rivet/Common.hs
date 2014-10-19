module Rivet.Common where

import           Prelude                 hiding ((++))

import           Control.Applicative
import           Data.Char
import           Data.Configurator.Types
import           Data.Monoid
import           Development.Shake
import           System.Exit
import           System.IO
import           System.Process

(++) :: Monoid a => a -> a -> a
(++) = mappend

data Task = Task { taskName    :: String
                 , taskNumArgs :: Int
                 , taskBody    :: String -> Config -> [String] -> Action ()
                 , taskUsage   :: String
                 }

exec :: String -> Action ExitCode
exec c = liftIO $ do putStrLn c
                     system c

readExec :: String -> Action String
readExec c = liftIO $ do (_,Just out,_, ph) <- createProcess $ (shell c) { std_out = CreatePipe }
                         waitForProcess ph
                         hGetContents out

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace


getDockerTag :: String -> String -> String -> Action String
getDockerTag proj h env = stripWhitespace <$> readExec ("ssh " ++ h ++ " \"docker ps\" | grep " ++ proj ++ "_" ++ env ++ "_ | awk '{ print $2}' | cut -d ':' -f 2 | head -n1")
