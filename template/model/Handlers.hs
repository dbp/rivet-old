{-# LANGUAGE OverloadedStrings #-}
module MODEL.Handlers where

import           Prelude              hiding ((++))

import           Snap.Plus
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap

import           Application
import           MODEL.Forms
import           MODEL.Splices
import           MODEL.Types

top :: AppHandler ()
top = route [("new", newH)
            ,(":id", routeMODEL)]
  where routeMODEL = do i <- getParam "id"
                        mODEL <- require $ getById i
                        route [("", ifTop $ showH mODEL)
                              ]


newH :: AppHandler ()
newH = do r <- runForm "new" newForm
          case r of
            (v, Nothing) -> renderWithSplices "mODEL/new" (digestiveSplices v)
            (_, Just t) -> do t' <- newMODEL t
                              redirect $ maybe "/" mODELPath t'

showH :: MODEL -> AppHandler ()
showH t = renderWithSplices "mODEL/show" (splices t)
