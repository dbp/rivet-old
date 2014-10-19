{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Application where

import           Control.Lens
import           Control.Monad.Logger
import           "mtl" Control.Monad.State           (get)
import           Control.Monad.Trans           (liftIO)
import           Data.Pool
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection)
import           Database.Redis                (Redis)
import           Network.DNS.Resolver
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple hiding (Query)
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Session


data App = App
     { _heist   :: Snaplet (Heist App)
     , _sess    :: Snaplet SessionManager
     , _db      :: Snaplet Postgres
     , _redis   :: Snaplet RedisDB
     , _dns     :: ResolvSeed
     , _siteUrl :: Text
     , _env     :: Text
     }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get

runRedis :: Redis a -> AppHandler a
runRedis = runRedisDB redis

instance HasDns AppHandler where
  getDnsSeed = use dns

type AppHandler = Handler App App
