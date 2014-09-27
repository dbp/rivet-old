{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = ""

runDown = ""
