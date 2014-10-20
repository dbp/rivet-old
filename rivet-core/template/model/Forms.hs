{-# LANGUAGE OverloadedStrings #-}
module MODEL.Forms where

import           Application
import           Snap.Plus
import           Snap.Plus.Forms
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap

import           MODEL.Types

newForm :: Form Text AppHandler MODELNew
newForm = MODEL' () <$> MFORM

editForm :: MODEL -> Form Text AppHandler MODEL
editForm (MODEL' id' mFIELDS) = MODEL' id' <$> MEDITFORM
