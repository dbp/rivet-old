{-# LANGUAGE OverloadedStrings #-}
module Rivet.Migration.V0 (
    Migration
  , createTable
  , dropTable
  , renameColumn
  , addColumn
  , dropColumn
  , sql
  , ColumnSpec(..)
  ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tuple
import           Database.PostgreSQL.Simple

import           Rivet.Migration

data ColumnSpec = ColumnSpec { colName        :: Text
                             , colType        :: Text
                             , colDefault     :: Maybe Text
                             , colConstraints :: Maybe Text
                             }

add :: (Text, Text) -> Migration ()
add p = Migration () [p]

invert :: Migration () -> Migration ()
invert (Migration () ps) = Migration () (map swap ps)

stripDown :: Migration () -> Migration ()
stripDown (Migration () ps) = Migration () (map (\(a,_) -> (a,"")) ps)

createTable :: Text -> [ColumnSpec] -> Migration ()
createTable tab cols = do add ("CREATE TABLE " <> tab <> "()", "DROP TABLE " <> tab)
                          stripDown $ mapM_ (addColumn tab) cols

-- NOTE(dbp 2014-10-18): To make this invertable, you need to pass in
-- the spec for how the table should be recreated. Obviously this is
-- reasonably unsafe, as we aren't checking that it looks like that
-- currently (so up and down may not be inverses if you mess that up).
dropTable :: Text -> [ColumnSpec] -> Migration ()
dropTable tab = invert . createTable tab

renameColumn :: Text -> Text -> Text -> Migration ()
renameColumn tab old new =
  add ("ALTER TABLE " <> tab <> " RENAME COLUMN " <> old <> " TO " <> new
      ,"ALTER TABLE " <> tab <> " RENAME COLUMN " <> new <> " TO " <> old)

addColumn :: Text -> ColumnSpec -> Migration ()
addColumn tab (ColumnSpec nm ty def constr) =
  add ("ALTER TABLE " <> tab <> " ADD COLUMN " <> nm <>
          " " <> ty <> maybe "" (" DEFAULT " <>) def <> " " <> fromMaybe "" constr,
       "ALTER TABLE " <> tab <> " DROP COLUMN " <> nm)

-- NOTE(dbp 2014-10-18): Like with 'dropTable', we have to specify
-- what the column should look like when you re-add it in order to
-- build the inverse.
dropColumn :: Text -> ColumnSpec -> Migration ()
dropColumn tab = invert . addColumn tab

sql :: Text -> Text -> Migration ()
sql up down = add (up, down)
