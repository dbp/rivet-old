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
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Heist
import qualified Heist.Interpreted                           as I
import           Heist.Splices.BindStrict
import           Heist.Splices.Ignore
import           Network.DNS.Resolver
import           Prelude                                     hiding ((++))
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple.Plus
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Text.XmlHtml                                as X

import           Application
import           Snap.Plus

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
                  hcInterpretedSplices = (do "currentPath" ## pathSplice
                                             siteSplices) }
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

prefixUrlSplice :: I.Splice AppHandler
prefixUrlSplice = do node <- getParamNode
                     case X.getAttribute "url" node of
                       Nothing -> return []
                       Just u -> lift $ ifIsUrl u (return $ X.elementChildren node) (return [])

suffixUrlSplice :: I.Splice AppHandler
suffixUrlSplice = do node <- getParamNode
                     case X.getAttribute "url" node of
                       Nothing -> return []
                       Just u -> do url <- fmap (T.takeWhile (/= '?') . T.decodeUtf8 . rqURI) getRequest
                                    return $ if u `T.isSuffixOf` url
                                              then X.elementChildren node
                                              else []

pathSplice :: I.Splice AppHandler
pathSplice = do path' <- getCurrentPath
                return [X.TextNode path']

siteSplices :: Splices (I.Splice AppHandler)
siteSplices = do "prefix-url" ## prefixUrlSplice
                 "suffix-url" ## suffixUrlSplice
                 bindStrictTag ## bindStrictImpl
                 ignoreTag ## ignoreImpl
