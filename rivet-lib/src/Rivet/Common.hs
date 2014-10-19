module Rivet.Common (
       (++), exec, readExec, stripWhitespace
 ) where

import           Data.Char
import           Data.Monoid
import           Development.Shake
import           Prelude           hiding ((++))
import           System.Exit
import           System.IO
import           System.Process

(++) :: Monoid a => a -> a -> a
(++) = mappend

exec :: String -> Action ExitCode
exec c = liftIO $ do putStrLn c
                     system c

readExec :: String -> Action String
readExec c = liftIO $ do (_,Just out,_, ph) <- createProcess $ (shell c) { std_out = CreatePipe }
                         waitForProcess ph
                         hGetContents out

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
