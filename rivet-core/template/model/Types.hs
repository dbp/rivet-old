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

data MODEL' a MVARS = MODEL' { id :: a MFIELDS }

type MODEL'' f = MODEL' (f Int) MTYPES_F
type MODEL = MODEL'' I
type MODELMaybe = MODEL'' Maybe
type MODELSpec = MODEL'' (Con (Wire String))
type MODELWire = MODEL'' Wire
type MODELMaybeWire = MODEL'' MaybeWire
type MODELNew = MODEL' ()

$(makeAdaptorAndInstance "pMODEL" ''MODEL')

mODELPath :: MODEL -> Text
mODELPath (MODEL' i) = "/mODEL/" ++ i

mODELsTable :: Table MODELWire
mODELsTable = Table "mODELs" (MODEL' (Wire "id") MWIRES)

allMODELs :: Query MODELWire
allMODELs = queryTable mODELsTable

getAllMODELs :: AppHandler [MODEL]
getAllMODELs = runO allMODELs

mODELsById :: Int -> Query MODELWire
mODELsById i = proc () ->
  do mODEL <- allMODELs -< ()
     i' <- constant i -< ()
     restrict <<< eq -< (id mODEL, i')
     returnA -< mODEL

getById :: Int -> AppHandler (Maybe MODEL)
getById i = listToMaybe <$> runO (mODELsById i)

newMODEL :: MODELNew -> AppHandler (Maybe MODEL)
newMODEL (MODEL' _ mFIELDS) = listToMaybe <$> insOR mODELsTable insE retE
  where insE :: Expr MODELMaybeWire
        insE = makeMaybeExpr (MODEL' Nothing mJUSTS :: MODELMaybe)
        retE :: ExprArr ListWire ListWire
        retE = proc mODEL -> returnA -< mODEL

updateMODEL :: MODEL -> AppHandler Bool
updateMODEL m@(MODEL' id' mFIELDS) = void $ updO mODELsTable updE (mODELsById id')
  where updE :: ExprArr MODELWire MODELMaybeWire
        updE = makeJustExpr m <<< arr (const ())
