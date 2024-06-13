{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (
  getApplicationDev,
  appMain,
  develMain,
  makeFoundation,
  makeLogWare,
  getAppSettings,

  -- * for DevelMain
  getApplicationRepl,
  shutdownApp,

  -- * for GHCI
  handler,
  db,
) where

import Config
import Control.Monad.Logger (liftLoc, runLoggingT)
import Data.CaseInsensitive (mk)
import Database.Persist.Postgresql (
  SqlBackend,
  createPostgresqlPool,
  pgConnStr,
  pgPoolSize,
 )
import Import hiding (sendResponse)
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (ResponseHeaders, status200)
import Network.Wai (Middleware, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (
  Settings,
  defaultSettings,
  defaultShouldDisplayException,
  getPort,
  runSettings,
  setHost,
  setOnException,
  setPort,
 )
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (
  Destination (Logger),
  IPAddrSource (..),
  OutputFormat (..),
  destination,
  mkRequestLogger,
  outputFormat,
 )
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import Text.Regex.Posix ((=~))

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Api.Handler.Arkham.Cards
import Api.Handler.Arkham.Decks
import Api.Handler.Arkham.Game.Debug
import Api.Handler.Arkham.Games
import Api.Handler.Arkham.Investigators
import Api.Handler.Arkham.PendingGames
import Api.Handler.Arkham.Replay
import Api.Handler.Arkham.Undo
import Base.Api.Handler.Authentication
import Base.Api.Handler.CurrentUser
import Base.Api.Handler.PasswordReset
import Base.Api.Handler.Registration
import Base.Api.Handler.Settings
import Control.Concurrent (forkIO)
import Data.List (lookup)
import Database.Redis (
  checkedConnect,
  newPubSubController,
  pubSubForever,
 )
import Handler.Health

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

{- | This function allocates resources (such as a database connection pool),
 performs initialization and returns a foundation datatype value. This is also
 the place to put your migrate statements to have automatic database
 migrations handled by Yesod.
-}
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

  appGameRooms <- newIORef mempty

  appRedis <- checkedConnect (appRedisConnectionInfo appSettings)
  appPubSub <- newPubSubController [] []
  _ <- forkIO $ pubSubForever appRedis appPubSub (pure ())

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger

  -- Create the database connection pool
  pool <-
    flip runLoggingT logFunc
      $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

  -- Perform database migration using our application's logging settings.
  -- runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

  -- Return the foundation
  pure $ mkFoundation pool

{- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
 applying some additional middlewares.
-}
makeApplication :: App -> IO Application
makeApplication foundation =
  makeMiddleware foundation <*> toWaiAppPlain foundation

makeMiddleware :: App -> IO Middleware
makeMiddleware foundation = do
  logWare <- makeLogWare foundation
  pure $ gzip def . logWare . handleOptions . addCORSHeaders

corsResponseHeaders :: ByteString -> [(ByteString, ByteString)]
corsResponseHeaders origin =
  [ ("Access-Control-Allow-Origin", validatedOrigin)
  , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE, PATCH")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Allow-Headers", "Content-Type, *")
  ,
    ( "Access-Control-Expose-Headers"
    , "Set-Cookie, Content-Disposition, Link, X-Echo"
    )
  , ("Cache-Control", "no-cache, no-store, max-age=0, private")
  ]
 where
  validOriginRegex = ".*" :: String
  validatedOrigin = if origin =~ validOriginRegex then origin else "BADORIGIN"

handleOptions :: Middleware
handleOptions app req sendResponse =
  case (requestMethod req, lookup "Origin" (requestHeaders req)) of
    ("OPTIONS", Just origin) ->
      sendResponse
        $ responseLBS status200 (toHeaders $ corsResponseHeaders origin) mempty
    _ -> app req sendResponse
 where
  toHeaders :: [(ByteString, ByteString)] -> ResponseHeaders
  toHeaders = map (first mk)

addCORSHeaders :: Middleware
addCORSHeaders app req sendResponse =
  case lookup "Origin" (requestHeaders req) of
    Nothing -> app req sendResponse
    Just origin -> addHeaders (corsResponseHeaders origin) app req sendResponse

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else
              Apache
                ( if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket
                )
      , destination = Logger $ loggerSet $ appLogger foundation
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException
      ( \_req e ->
          when (defaultShouldDisplayException e)
            $ messageLoggerSource
              foundation
              (appLogger foundation)
              $(qLocation >>= liftLoc)
              "yesod"
              LevelError
              (toLogStr $ "Exception from Warp: " ++ show e)
      )
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  pure (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings ["config/settings.yml"] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <-
    loadYamlSettingsArgs
      -- fall back to compile-time values, set to [] to require values at runtime
      [configSettingsYmlValue]
      -- allow environment variables to override
      useEnv

  -- Generate the foundation from the settings
  foundation <- makeFoundation settings

  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation

  -- Run the application with Warp
  runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  pure (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = pure ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
