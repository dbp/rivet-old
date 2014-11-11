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


data Direction = Up | Down

run :: String -> Connection -> Direction -> Migration () -> IO ()
run nm conn dir m =
  do mapM_ (\p -> do let str = T.unpack $ pick dir p
                     case str of
                       "" -> return ()
                       _ -> do execute_ conn (fromString str)
                               putStrLn str)
           (migSteps m)
     case dir of
       Up -> execute conn "INSERT INTO migrations (name) values (?)" (Only nm)
       Down -> execute conn "DELETE FROM migrations WHERE name = ?" (Only nm)
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
