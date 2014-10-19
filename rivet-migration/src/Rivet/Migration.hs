{-# LANGUAGE OverloadedStrings #-}
module Rivet.Migration where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tuple
import           Database.PostgreSQL.Simple

-- NOTE(dbp 2014-10-18): step is a pair of up,down queries.
data Migration v = Migration { migValue :: v, migSteps :: [(Text, Text)]}
data ColumnSpec = ColumnSpec { colName        :: Text
                             , colType        :: Text
                             , colDefault     :: Maybe Text
                             , colConstraints :: Maybe Text
                             }


data Direction = Up | Down

run :: String -> Connection -> Direction -> Migration () -> IO ()
run nm conn dir m =
  do mapM_ (\p -> execute_ conn (fromString . T.unpack $ pick dir p)) (migSteps m)
     execute conn "INSERT INTO migrations (name) values (?)" (Only nm)
     return ()
  where pick Up (sql,_) = sql
        pick Down (_,sql) = sql

instance Functor Migration where
  fmap f m = m { migValue = f (migValue m) }

instance Applicative Migration where
  pure v = Migration v []
  (<*>) (Migration f ss) (Migration v ss') = Migration (f v) (ss ++ ss')

instance Monad Migration where
  (>>=) (Migration v ss) f = let (Migration v' ss') = f v in Migration v' (ss ++ ss')
  return v = Migration v []

add :: (Text, Text) -> Migration ()
add p = Migration () [p]

invert :: Migration () -> Migration ()
invert (Migration () ps) = Migration () (map swap ps)

createTable :: Text -> [ColumnSpec] -> Migration ()
createTable tab cols = do add ("CREATE TABLE " <> tab, "DROP TABLE " <> tab)
                          mapM_ (addColumn tab) cols

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
-- what tho column should look like when you re-add it in order to
-- build the inverse.
dropColumn :: Text -> ColumnSpec -> Migration ()
dropColumn tab = invert . addColumn tab

sql :: Text -> Text -> Migration ()
sql up down = add (up, down)
