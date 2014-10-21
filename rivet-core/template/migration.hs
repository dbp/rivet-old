{-# LANGUAGE OverloadedStrings #-}
module MIGRATION_MODULE where

import           Control.Monad
import           Rivet.Migration

migrate = sql up down

up = ""

down = ""
