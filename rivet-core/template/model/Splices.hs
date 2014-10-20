{-# LANGUAGE OverloadedStrings #-}
module MODEL.Splices where

import           Heist
import           Heist.Interpreted
import           Snap.Plus

import           Application
import           MODEL.Types

splices :: MODEL -> Splices (Splice AppHandler)
splices (MODEL' i mFIELDS) =
  do "id" ## textSplice $ tshow i
MSPLICES
