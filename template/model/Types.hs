{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module MODEL.Types where

import           Prelude           hiding (id, (++))

import           Application
import           Data.Maybe
import           Data.Text         (Text)
import           Snap.Plus
import           Snap.Plus.Opaleye

data MODEL' a b c = MODEL' { id :: a
                           }

type MODEL'' f = MODEL' (f Int)
type MODEL = MODEL'' I
type MODELMaybe = MODEL'' Maybe
type MODELSpec = MODEL'' (Con (Wire String))
type MODELWire = MODEL'' Wire
type MODELMaybeWire = MODEL'' MaybeWire
type MODELNew = MODEL' ()

$(makeAdaptorAndInstance "pMODEL" ''MODEL')

mODELPath :: MODEL -> Text
mODELPath (MODEL' i) = "/" ++ mODEL ++ "/" ++ i

mODELsTable :: Table MODELWire
mODELsTable = Table "mODELs" (MODEL' (Wire "id"))

allMODELs :: Query MODELWire
allMODELs = queryTable mODELsTable

getAllMODELs :: AppHandler [MODEL]
getAllMODELs = runO allMODELs

mODELsById :: Int -> Query MODELWire
mODELsById i = proc () -> do mODEL <- allMODELs -< ()
                             i' <- constant i -< ()
                             restrict <<< eq -< (id mODEL, i')
                             returnA -< mODEL

getById :: Int -> AppHandler (Maybe MODEL)
getById i = listToMaybe <$> runO (mODELsById i)

newMODEL :: MODELNew -> AppHandler (Maybe MODEL)
newMODEL (MODEL' _ ) = listToMaybe <$> insOR mODELsTable insE retE
  where insE :: Expr MODELMaybeWire
        insE = makeMaybeExpr (MODEL' Nothing :: MODELMaybe)
        retE :: ExprArr ListWire ListWire
        retE = proc mODEL -> returnA -< mODEL
