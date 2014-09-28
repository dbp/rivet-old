{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app, routes
  ) where

import           Control.Monad.State
import           Data.ByteString                             (ByteString)
import qualified Data.Configurator                           as C
import           Data.Monoid
import           Network.DNS.Resolver
import           Prelude                                     hiding ((++))
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple.Plus
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           Application
import           Snap.Plus
import           Snap.Plus.Splices

routes :: [(ByteString, AppHandler ())]
routes = [ ("",       heistServe)
         , ("",       serveDirectory "static")
         , ("",       do modifyResponse (setResponseCode 404)
                         render "notfound")
         ]


app :: SnapletInit App App
app = makeSnaplet "app" "" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "templates"
         mempty { hcLoadTimeSplices = defaultLoadTimeSplices,
                  hcInterpretedSplices = plusSplices }
    conf <- getSnapletUserConfig
    url <- liftIO (C.require conf "siteUrl")
    absPath <- liftIO (C.lookupDefault "" conf "absolutePath")
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager (absPath ++ "site_key.txt") "sess" Nothing
    d <- nestSnaplet "db" db pgsInit
    r <- nestSnaplet "redis" redis redisDBInitConf
    ns <- liftIO $ makeResolvSeed defaultResolvConf
    e <- getEnvironment
    addRoutes routes
    return $ App h s d r ns url (T.pack e)
