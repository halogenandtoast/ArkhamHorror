{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (
  appMain,
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

import Api.Arkham.Archive (archiveSweeper, restoreWorker)
import Api.Arkham.Helpers (
  applyMaintenance,
  loadMaintenance,
  markPubSubAlive,
  onMaintenanceMessage,
  onRoomOpMessage,
  pubSubHealthChannel,
  pubSubSupervisor,
  roomHeartbeat,
  roomSourcesFrom,
 )
import Api.Arkham.Maintenance (maintenanceChannel)
import Api.Arkham.RoomOps (roomOpsChannel)
import Arkham.Metrics qualified as Metrics
import Config
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Logger (liftLoc, runLoggingT)
import Data.Bugsnag.Settings qualified as Bugsnag
import Data.CaseInsensitive (foldCase, mk)
import Data.Default.Class (def)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.X509.CertificateStore (readCertificateStore)
import Database.Persist.Postgresql (
  SqlBackend,
  createPostgresqlPool,
  pgConnStr,
  pgPoolSize,
 )
import Database.Redis (
  ConnectAddr (..),
  ConnectInfo (..),
  checkedConnect,
  newPubSubController,
  parseConnectInfo,
 )
import Import hiding (newMVar, sendResponse)
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (ResponseHeaders, status200, status304)
import Network.TLS (ClientParams (..), Shared (..), Supported (..), defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Network.Wai (
  Middleware,
  mapResponseHeaders,
  requestHeaders,
  requestMethod,
  responseHeaders,
  responseLBS,
  responseStatus,
 )
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
import Network.Wai.Middleware.Gzip (gzip)
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

import Api.Handler.ApiKeys
import Api.Handler.Arkham.Achievements
import Api.Handler.Arkham.Admin.Archives
import Api.Handler.Arkham.Admin.CustomCardSets
import Api.Handler.Arkham.Admin.GameStats
import Api.Handler.Arkham.Admin.Metrics
import Api.Handler.Arkham.Admin.Users
import Api.Handler.Arkham.Archive
import Api.Handler.Arkham.BuildConnection
import Api.Handler.Arkham.CardSetShares
import Api.Handler.Arkham.CardSetSubmissions
import Api.Handler.Arkham.Cards
import Api.Handler.Arkham.CustomCardSets
import Api.Handler.Arkham.CustomCards
import Api.Handler.Arkham.Decks
import Api.Handler.Arkham.Events
import Api.Handler.Arkham.Game.Bug
import Api.Handler.Arkham.Game.Debug
import Api.Handler.Arkham.Game.LogExport
import Api.Handler.Arkham.Games
import Api.Handler.Arkham.Games.Admin
import Api.Handler.Arkham.Investigators
import Api.Handler.Arkham.Maintenance
import Api.Handler.Arkham.Old
import Api.Handler.Arkham.PendingGames
import Api.Handler.Arkham.PublishedCardSets
import Api.Handler.Arkham.Replay
import Api.Handler.Arkham.Undo
import Api.Handler.ThirdEdition
import Base.Api.Handler.Account
import Base.Api.Handler.Authentication
import Base.Api.Handler.CurrentUser
import Base.Api.Handler.Notifications
import Base.Api.Handler.PasswordReset
import Base.Api.Handler.Registration
import Base.Api.Handler.Settings
import Control.Concurrent (forkIO)
import Handler.Health
import ThirdEdition.Store qualified as ThirdEdition

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

{- | A name for this replica, for the cross-pod room ops in "Api.Arkham.RoomOps".

@HOSTNAME@ is the pod name under Kubernetes, which is the name an operator
already has in front of them in @kubectl@. The suffix is this process: a pod
that restarts keeps its name, and an admin comparing two answers five minutes
apart should be able to see that the pod behind them is not the same one.
-}
currentPodId :: IO Text
currentPodId = do
  podHost <- lookupEnv "HOSTNAME"
  podSuffix <- T.take 6 . UUID.toText <$> UUID.nextRandom
  pure $ maybe "pod" T.pack podHost <> "/" <> podSuffix

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
  let appBugsnag = Bugsnag.defaultSettings (appBugsnagApiKey appSettings)

  appGameRooms <- newMVar mempty
  appEventRooms <- newMVar mempty
  appThirdEditionRooms <- newMVar mempty
  appPubSubHealth <- newTVarIO =<< getCurrentTime
  appMaintenance <- newTVarIO Nothing
  appPodId <- currentPodId

  -- Closed over by the pub/sub handler below, which is built before the
  -- foundation exists -- so it gets the room maps, not the 'App'.
  let maintenanceRooms =
        roomSourcesFrom appGameRooms appEventRooms appThirdEditionRooms

  appMessageBroker <- case appRedisConnectionInfo appSettings of
    Nothing -> pure WebSocketBroker
    Just url -> do
      conn <- checkedConnect =<< fromConnectionUrl url
      -- Come up in whatever mode the fleet is already in: a pod that started
      -- during a maintenance window must not serve traffic into a locked
      -- table just because it missed the announcement.
      loadMaintenance conn >>= applyMaintenance appMaintenance maintenanceRooms
      -- The health and maintenance channels are INITIAL subscriptions so
      -- 'pubSubForever' restores them on every reconnect and the controller is
      -- never left with zero channels. Per-game channels are added and removed
      -- on demand by 'getRoomIn' / 'releaseRoomIfEmpty'.
      ctrl <-
        newPubSubController
          [ (pubSubHealthChannel, const $ markPubSubAlive appPubSubHealth)
          , (maintenanceChannel, onMaintenanceMessage appMaintenance maintenanceRooms)
          , (roomOpsChannel, onRoomOpMessage conn appPodId appGameRooms)
          ]
          []
      -- Supervised, not bare: 'pubSubForever' exits on connection loss and
      -- must be restarted to resubscribe, and it cannot see a half-open
      -- socket at all. See 'pubSubSupervisor'.
      _ <- forkIO $ pubSubSupervisor appPubSubHealth conn ctrl
      pure $ RedisBroker conn ctrl

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool appThirdEditionStore = App {..}
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation =
        mkFoundation
          (error "connPool forced in tempFoundation")
          (error "thirdEditionStore forced in tempFoundation")
      logFunc = messageLoggerSource tempFoundation appLogger

  -- Create the database connection pool
  pool <-
    flip runLoggingT logFunc
      $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

  -- Perform database migration using our application's logging settings.
  -- runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

  -- Third-edition tables are state, so they live in the database.
  thirdEditionStore <- ThirdEdition.newPostgresStore pool

  let foundation = mkFoundation pool thirdEditionStore

  -- Per-pod room upkeep, on one timer. Refreshes the 'arkham:rooms:seen'
  -- timestamps for any game this pod is still serving, so admin counts age out
  -- automatically when a pod crashes (that half needs Redis and is skipped
  -- without it), and closes the rooms whose players have all left, which does
  -- not.
  _ <- forkIO (roomHeartbeat foundation)

  -- The daily sweep that offloads old games' undo history, and the worker that
  -- picks up a restore whose replica died mid-download. Both poll, both no-op
  -- when no archive bucket is configured, and the sweep is claimed in the
  -- database so only one replica runs it. See "Api.Arkham.Archive".
  _ <- forkIO (archiveSweeper foundation)
  _ <- forkIO (restoreWorker foundation)

  pure foundation

{- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
 applying some additional middlewares.
-}
makeApplication :: App -> IO Application
makeApplication foundation =
  makeMiddleware foundation <*> toWaiAppPlain foundation

makeMiddleware :: App -> IO Middleware
makeMiddleware foundation = do
  logWare <- makeLogWare foundation
  pure $ gzip def . skipWebSocketLogging logWare . handleOptions . addCORSHeaders

{- | Don't run the access logger for websocket upgrades. 'mkRequestLogger' logs
once @sendResponse@ returns, which for a hijacked connection is when the
socket *closes*, and a raw response has no HTTP status so every game socket
shows up as a bogus @500@. Neither is useful, and the game stream is chatty
enough to bury real requests.
-}
skipWebSocketLogging :: Middleware -> Middleware
skipWebSocketLogging logWare app req sendResponse
  | isWebSocketUpgrade = app req sendResponse
  | otherwise = logWare app req sendResponse
 where
  isWebSocketUpgrade =
    maybe False ((== "websocket") . foldCase)
      $ lookup "Upgrade" (requestHeaders req)

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

{- | CORS headers, and the no-caching default that rides with them.

Appending, as this does, means a handler that has set its own 'Cache-Control' would end
up with two -- and since the default here says @no-store@, the strictest of the
pair is the one a browser keeps, which is the opposite of what the handler asked
for. So the default is dropped for a response that already carries one.

That is what lets an endpoint opt into revalidation (an @ETag@ plus
@private, no-cache@, as 'Api.Handler.Arkham.CustomCards.setPayloadEtag' does)
without changing what every other endpoint promises, which is still that nothing
is stored anywhere.
-}
addCORSHeaders :: Middleware
addCORSHeaders app req sendResponse =
  case lookup "Origin" (requestHeaders req) of
    Nothing -> app req sendResponse
    Just origin -> app req \res ->
      let
        -- 'HeaderName' is case-insensitive in its own Eq, so this needs no folding.
        setsOwnCacheControl = any ((== "cache-control") . fst) (responseHeaders res)
        {- A 304 is the answer to a revalidation, and it carries no body to say
        anything about. Telling the reader not to store it would be read as
        applying to the entry it just revalidated, which is the copy the 304
        exists to let them keep. -}
        revalidated = responseStatus res == status304
        headers =
          [ (mk name, value)
          | (name, value) <- corsResponseHeaders origin
          , not ((setsOwnCacheControl || revalidated) && foldCase name == "cache-control")
          ]
       in
        sendResponse $ mapResponseHeaders (headers <>) res

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

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings ["config/settings.yml"] [] useEnv

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

  -- Opt-in performance metrics collector. Set ARKHAM_METRICS=1 (or any non-empty
  -- value besides "0"/"false") to enable global span timing; query the
  -- /api/v1/admin/metrics endpoint to read or reset the table.
  metricsEnv <- lookupEnv "ARKHAM_METRICS"
  case metricsEnv of
    Just v | v /= "" && v /= "0" && v /= "false" -> void Metrics.enableMetrics
    _ -> pure ()

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

{- | hedis 0.16 replaced ConnectInfo's connectHost/connectPort pair with a
single connectAddr. Only the host/port form can carry TLS, but keep this
total by falling back to the socket path.
-}
connectAddrHostName :: ConnectAddr -> String
connectAddrHostName = \case
  ConnectAddrHostPort host _ -> host
  ConnectAddrUnixSocket path -> path

-- parse a text url into a redis connection
fromConnectionUrl :: (MonadFail m, MonadIO m) => Text -> m ConnectInfo
fromConnectionUrl info = do
  case parseConnectInfo (T.unpack $ T.replace "rediss://" "redis://" info) of
    Right x ->
      if "rediss" `T.isPrefixOf` info
        then do
          Just certStore <- liftIO $ readCertificateStore "digital-ocean.crt"
          pure
            $ x
              { connectTLSParams =
                  Just
                    $ (defaultParamsClient (connectAddrHostName $ connectAddr x) "")
                      { clientSupported = def {supportedCiphers = ciphersuite_strong}
                      , clientShared = def {sharedCAStore = certStore}
                      }
              }
        else pure x
    Left err -> fail err
