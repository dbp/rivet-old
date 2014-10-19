module Main where

import           Rivet.Main

import qualified Rivet.Lib.SimpleDeploy

main :: IO ()
main = mainWith Rivet.Lib.SimpleDeploy.tasks
