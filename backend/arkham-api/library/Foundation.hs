{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Foundation where

import Import.NoFoundation

import Control.Concurrent.STM.TBQueue
import Control.Exception.Annotated qualified as UE
import Control.Exception.Annotated.UnliftIO qualified as AnnotatedIO
import Control.Monad.Catch (
  MonadCatch (..),
  MonadMask (..),
  MonadThrow (..),
  generalBracket,
 )
import Control.Monad.Catch qualified as Catch
import Data.IntMap.Strict qualified as IntMap
import UnliftIO.Exception qualified as UnliftIO

import Api.Arkham.Maintenance
import Arkham.Card.CardCode
import Arkham.Log.Entry (LogRow)
import Auth.ApiKey qualified as ApiKey
import Auth.Feature qualified as Feature
import Auth.JWT qualified as JWT
import Control.Monad.Logger (LogSource)
import Data.Aeson (Result (Success), fromJSON)
import Data.Bugsnag.Settings qualified as Bugsnag
import Data.ByteString.Lazy qualified as BSL
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.UUID (UUID)
import Database.Persist.Sql (
  ConnectionPool,
  SqlBackend,
  SqlPersistT,
  runSqlPool,
 )
import Database.Redis (
  Connection,
  PubSubController,
  RedisChannel,
 )
import GHC.Records
import Network.Bugsnag.Exception (AsException (..))
import Network.Bugsnag.Yesod (bugsnagYesodMiddleware)
import Network.HTTP.Client.Conduit (HasHttpManager (..), Manager)
import Network.HTTP.Types (status429, status503)
import Orphans ()
import ThirdEdition.Store qualified as ThirdEdition
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe
import "bugsnag" Network.Bugsnag qualified as Bugsnag
import "bugsnag-hs" Network.Bugsnag qualified as Bugsnag (Exception)

{- | Per-subscriber bounded queue. When the queue fills up we treat the
subscriber as unable to keep up and drop further writes for it. The
subscriber's reader loop in 'gameStream' cancels itself once it observes
the overflow flag, which closes the WebSocket and triggers cleanup.
-}
data Subscriber = Subscriber
  { subQueue :: TBQueue BSL.ByteString
  , subOverflow :: TVar Bool
  , subClosed :: TVar Bool
  {- ^ Set when the room this socket is attached to has been deleted. The
  sender loop drains whatever is still queued and then ends, which drops out
  of its 'race_' and closes the connection -- so a client cannot keep a
  deleted room alive, and cannot be left attached to one that no longer
  exists. See 'closeRoomSubscribers'.
  -}
  }

data Room = Room
  { roomSubscribers :: TVar (IntMap Subscriber)
  , roomNextSubId :: TVar Int
  , messageBrokerChannel :: RedisChannel
  , roomLogCache :: TVar (Maybe RoomLogCache)
  {- ^ In-memory mirror of arkham_log_entries for this game, so updateGame
  doesn't have to re-read the entire log from the DB on every action.
  Validated against the locked game's step on each use; if the step
  doesn't match (another server modified the game), we fall back to
  reading from the DB and refresh the cache.
  -}
  , roomUnsubscribe :: TVar (IO ())
  {- ^ Tears down this room's single Redis subscription. There is exactly one
  subscription per channel per pod, owned by the room rather than by any
  one WebSocket, and it is created and destroyed under the rooms 'MVar'
  (see 'getRoomIn' / 'releaseRoomIfEmpty') so that "room is in the map"
  and "channel is subscribed" can never disagree. 'pure ()' when no Redis
  broker is configured.
  -}
  , roomWatchers :: TVar Int
  {- ^ How many of this room's sockets are spectators rather than seated
  players. Only this pod's share of them: with Redis the authoritative
  number is the cross-pod total in @arkham:watchers@, and this is the
  fallback for a single-pod deployment, which is every local dev setup.
  -}
  , roomNoPlayersSince :: TVar (Maybe Int)
  {- ^ Unix epoch at which this room was first seen holding watchers and no
  players, or 'Nothing' while a player is still on it. Read by the sweep
  in 'Api.Arkham.Helpers.sweepAbandonedRooms', which is why it is a
  timestamp and not a flag: a player reloading the page is a room with no
  players for a second or two, and only a room that stays that way is
  abandoned.
  -}
  }

{- | Cache of one game's log entries. 'cacheStep' is the arkham_games.step
value at the time we cached. Invalidate (refetch from DB) when the
locked game's step doesn't match.
-}
data RoomLogCache = RoomLogCache
  { cacheStep :: !Int
  , cacheEntries :: ![LogRow]
  {- ^ A bounded tail, not the whole history; see
  'Api.Arkham.Helpers.gameLogTailSize'.
  -}
  }

instance HasField "broker" Room RedisChannel where
  getField = messageBrokerChannel

{- | Maximum messages buffered per WebSocket subscriber before we mark them
as overflowed. Tuned to bound memory in the worst case (slow client,
half-open TCP connection): ~256 messages * a few KB each = a few MB per
stuck connection, after which the subscriber is torn down.
-}
roomQueueBound :: Natural
roomQueueBound = 256

newRoom :: RedisChannel -> IO Room
newRoom chn = atomically do
  subs <- newTVar IntMap.empty
  next <- newTVar 0
  cache <- newTVar Nothing
  unsub <- newTVar (pure ())
  watchers <- newTVar 0
  noPlayers <- newTVar Nothing
  pure $ Room subs next chn cache unsub watchers noPlayers

{- | Register a new WebSocket subscriber on the room. Returns the
subscription id (used to unsubscribe) and the bounded queue the
subscriber should read from.
-}
subscribeToRoom :: MonadIO m => Room -> m (Int, Subscriber)
subscribeToRoom room = liftIO $ atomically do
  q <- newTBQueue roomQueueBound
  overflow <- newTVar False
  closed <- newTVar False
  let sub = Subscriber q overflow closed
  subId <- readTVar (roomNextSubId room)
  writeTVar (roomNextSubId room) (subId + 1)
  modifyTVar' (roomSubscribers room) (IntMap.insert subId sub)
  pure (subId, sub)

unsubscribeFromRoom :: MonadIO m => Room -> Int -> m ()
unsubscribeFromRoom room subId = liftIO $ atomically do
  modifyTVar' (roomSubscribers room) (IntMap.delete subId)

{- | Evict every socket on the room.

Each sender loop finishes what it already has queued and then ends, so a
message broadcast immediately before this one -- the notice saying why -- still
reaches the client. Used when the room is being deleted rather than emptied:
'releaseRoomIfEmpty' is for the ordinary case where the last subscriber has
already left of their own accord.
-}
closeRoomSubscribers :: MonadIO m => Room -> m ()
closeRoomSubscribers room = liftIO $ atomically do
  subs <- readTVar (roomSubscribers room)
  for_ (IntMap.elems subs) \Subscriber {subClosed} -> writeTVar subClosed True

-- | Number of currently registered subscribers (used by the admin UI).
roomClientCount :: MonadIO m => Room -> m Int
roomClientCount room = liftIO $ atomically do
  IntMap.size <$> readTVar (roomSubscribers room)

{- | Move this pod's spectator tally for the room by @delta@ and read it back.

Clamped at zero: the counter is adjusted from a bracket's release, which can
run for a socket whose acquire never completed, and a table that claims -1
watchers is worse than one that claims none.
-}
adjustRoomWatchers :: MonadIO m => Room -> Int -> m Int
adjustRoomWatchers room delta = liftIO $ atomically do
  modifyTVar' (roomWatchers room) (max 0 . (+ delta))
  readTVar (roomWatchers room)

-- | This pod's spectator tally for the room.
roomWatcherCount :: MonadIO m => Room -> m Int
roomWatcherCount room = readTVarIO (roomWatchers room)

{- | Send a payload to every subscriber on the room. Subscribers whose
queue is full are flagged as overflowed and skipped; the corresponding
reader loop will observe the flag and tear itself down.
-}
broadcastToRoom :: MonadIO m => Room -> BSL.ByteString -> m ()
broadcastToRoom room msg = liftIO $ atomically do
  subs <- readTVar (roomSubscribers room)
  for_ (IntMap.elems subs) \Subscriber {subQueue, subOverflow} -> do
    overflowed <- readTVar subOverflow
    unless overflowed do
      full <- isFullTBQueue subQueue
      if full
        then writeTVar subOverflow True
        else writeTBQueue subQueue msg

data MessageBroker = WebSocketBroker | RedisBroker Connection PubSubController

{- | The foundation datatype for your application. This can be a good place to
keep settings and values requiring initialization before your application
starts running, such as database connections. Every handler will have
access to the data present here.
-}
data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
  , appMessageBroker :: MessageBroker
  -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  , appGameRooms :: !(MVar (Map ArkhamGameId Room))
  , appEventRooms :: !(MVar (Map ArkhamEpicEventId Room))
  {- ^ Epic Multiplayer: per-event websocket rooms (organizer dashboard feed),
  sibling of 'appGameRooms'.
  -}
  , appThirdEditionRooms :: !(MVar (Map UUID Room))
  -- ^ Third edition: per-table websocket rooms, sibling of 'appGameRooms'.
  , appThirdEditionStore :: ThirdEdition.Store
  , appPubSubHealth :: !(TVar UTCTime)
  {- ^ When this pod last saw a message arrive on the pub/sub subscriber
  socket. A half-open subscriber TCP connection is invisible to hedis --
  it stays blocked in @recv@ and never throws -- so 'pubSubSupervisor'
  publishes a heartbeat and watches this timestamp to decide whether the
  subscription is actually alive. See 'pubSubHealthChannel'.
  -}
  , appMaintenance :: !(TVar (Maybe Maintenance))
  {- ^ Maintenance mode for this pod: 'Just' while the app is deliberately
  refusing traffic so the database can be worked on. Seeded from Redis at
  startup and kept in step across replicas over 'maintenanceChannel', so it
  is a cache of a shared decision rather than per-pod state. Enforced by
  'maintenanceMiddleware'.
  -}
  , appBugsnag :: Bugsnag.Settings
  }

class Monad m => HasApp m where
  getApp :: m App

getsApp :: HasApp m => (App -> a) -> m a
getsApp f = f <$> getApp

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type). MonadIO m => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req -> case appRoot $ appSettings app of
    Nothing -> getApprootText guessApproot app req
    Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware =
    maintenanceMiddleware . bugsnagYesodMiddleware appBugsnag . defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout _ = pure ""

  isAuthorized
    :: Route App
    -- \^ The route the user is visiting.
    -> Bool
    -- \^ Whether or not this is a "write" request.
    -> Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized HealthR _ = pure Authorized
  isAuthorized ErrorR _ = pure Authorized
  isAuthorized (ApiP api) _ = case api of
    -- The OAuth callback arrives as a browser redirect with no credentials of
    -- its own; what it may do comes from the state it carries.
    ApiAuthArkhamBuildCallbackR -> pure Authorized
    ApiV1P v1 -> case v1 of
      AdminP _ -> do
        _ <- getAdminUser
        pure Authorized
      -- API keys are behind a per-account grant while they roll out. Gated here
      -- rather than in the handlers to match the admin routes above, and because
      -- a gate a new endpoint has to remember to apply is a gate that will be
      -- forgotten.
      --
      -- `self` is deliberately excluded: it is the one endpoint a *key* calls
      -- about itself, so the MCP server can ask what its credential may do and
      -- offer only the tools it can use. `getFeatureUser` goes through
      -- `getRequestUserId`, which is JWT-only, so gating it would 401 every key
      -- and take that away.
      ApiV1ApiKeysP keys -> case keys of
        ApiV1ApiKeySelfR -> pure Authorized
        _ -> do
          _ <- getFeatureUser Feature.apiKeys
          pure Authorized
      -- Authenticated by token inside the handler, deliberately not by
      -- 'getAdminUser': that reads the users table, which would make the one
      -- endpoint whose job is to lift a lock on the database the first thing
      -- to queue up behind it.
      ApiV1MaintenanceR -> pure Authorized
      ApiV1ArkhamP arkham -> case arkham of
        ApiV1ArkhamGamesP games -> case games of
          ApiV1ArkhamGamesImportR -> pure Authorized
          ApiV1ArkhamGamesFixR -> do
            _ <- getAdminUser
            pure Authorized
          ApiV1ArkhamGamesReloadR -> do
            _ <- getAdminUser
            pure Authorized
          ApiV1ArkhamGameP _ game -> case game of
            ApiV1ArkhamGameFullExportR -> do
              _ <- getAdminUser
              pure Authorized
            ApiV1ArkhamGameReloadR -> do
              _ <- getAdminUser
              pure Authorized
            _ -> pure Authorized
          _ -> pure Authorized
        _ -> pure Authorized
      _ -> pure Authorized

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    pure
      $ appShouldLogAll (appSettings app)
      || level
      == LevelWarn
      || level
      == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = pure . appLogger

  maximumContentLength :: App -> Maybe (Route App) -> Maybe Word64
  maximumContentLength app _
    | appReloadTemplates (appSettings app) = Nothing
    | otherwise = Just $ 200 * 1024 * 1024

class Monad m => CanRunDB m where
  runDB :: SqlPersistT m a -> m a

bugsnag :: (HasBugsnag m, MonadIO m) => Bugsnag.Exception -> m ()
bugsnag e = do
  settings <- bugsnagSettings
  liftIO $ Bugsnag.notifyBugsnag settings (AsException e)

class HasBugsnag m where
  bugsnagSettings :: m Bugsnag.Settings

instance HasApp m => HasBugsnag m where
  bugsnagSettings = getsApp appBugsnag

instance HasApp (HandlerFor App) where
  getApp = getYesod

instance CanRunDB (HandlerFor App) where
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

newtype ExceptionViaIO m a = ExceptionViaIO (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

{- | A variant of 'throwWithCallStackIO' that uses 'MonadIO' instead of
'MonadThrow'.
-}
throwWithCallStackIO
  :: (MonadIO m, Exception e)
  => e
  -> m a
throwWithCallStackIO = liftIO . withFrozenCallStack UE.throwWithCallStack

instance MonadIO m => MonadThrow (ExceptionViaIO m) where
  throwM = throwWithCallStackIO

instance MonadUnliftIO m => MonadCatch (ExceptionViaIO m) where
  catch = AnnotatedIO.catch

instance MonadUnliftIO m => MonadMask (ExceptionViaIO m) where
  generalBracket = generalBracketIO
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask

deriving via ExceptionViaIO (HandlerFor app) instance MonadMask (HandlerFor app)
deriving via ExceptionViaIO (HandlerFor app) instance MonadCatch (HandlerFor app)

generalBracketIO
  :: MonadUnliftIO m
  => m a
  -> (a -> Catch.ExitCase b -> m c)
  -> (a -> m b)
  -> m (b, c)
generalBracketIO acquire release action = do
  UnliftIO.mask \restore -> do
    x <- acquire
    res1 <- UnliftIO.try $ restore $ action x
    case res1 of
      Left e1 -> do
        -- explicitly ignore exceptions from after
        _ :: Either SomeException c <-
          UnliftIO.try $ UnliftIO.uninterruptibleMask_ $ release x (Catch.ExitCaseException e1)
        throwWithCallStackIO e1
      Right y -> do
        c <- UnliftIO.uninterruptibleMask_ $ release x (Catch.ExitCaseSuccess y)
        pure (y, c)

-- How to run database actions.
-- instance YesodPersist App where
--   type YesodPersistBackend App = SqlBackend
--   runDB :: SqlPersistT Handler a -> Handler a
--   runDB action = do
--     master <- getYesod
--     runSqlPool action $ appConnPool master
--
-- instance YesodPersistRunner App where
--   getDBRunner :: Handler (DBRunner App, Handler ())
--   getDBRunner = defaultGetDBRunner appConnPool

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

userIdToToken :: UserId -> HandlerFor App Text
userIdToToken userId = do
  jwtSecret <- getJwtSecret
  liftIO $ JWT.jsonToToken jwtSecret $ toJSON userId

tokenToUserId :: Text -> Handler (Maybe UserId)
tokenToUserId token = do
  jwtSecret <- getJwtSecret
  mUserId <- liftIO $ fmap fromJSON <$> JWT.tokenToJson jwtSecret token
  case mUserId of
    Just (Success userId) -> pure $ Just userId
    _ -> pure Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret = getsYesod $ appJwtSecret . appSettings

{- | The account owner, acting as themselves.

Deliberately JWT-only: an API key is accepted by 'getScopedCaller' and nowhere
else, so an endpoint added later is closed to keys until somebody says which
scope opens it. The alternative -- keys accepted everywhere, each handler
remembering to check -- fails open, and the failure is that a key granted for
writing cards can delete the account.
-}
getRequestUserId :: Handler UserId
getRequestUserId = do
  mToken <- JWT.lookupToken
  maybe notAuthenticated pure . join =<< for mToken tokenToUserId

{- | Who is calling, when the route does not require anyone to be.

For a route that answers everyone but can answer a signed-in caller with more:
the investigator list is the same for the world and carries your own custom
investigators on top. Deliberately not a key caller -- a route that widens its
answer for whoever is asking should widen it for the owner only.
-}
lookupRequestUserId :: Handler (Maybe UserId)
lookupRequestUserId = fmap join . traverse tokenToUserId =<< JWT.lookupToken

{- | Who is calling, and what they are allowed to do.

A JWT caller is the owner and carries every scope. A key caller carries only what
it was granted, and must hold all of @required@ or the request is refused -- 403
rather than 401, because the credential is good and the permission is not, and a
client that retries authentication on a 401 would otherwise loop.
-}
data Caller = Caller
  { callerUserId :: UserId
  , callerScopes :: [ApiKey.Scope]
  , callerApiKeyId :: Maybe ArkhamApiKeyId
  -- ^ Absent for the owner's own session.
  }

getScopedCaller :: [ApiKey.Scope] -> Handler Caller
getScopedCaller required = do
  mKey <- ApiKey.lookupApiKeyHeader
  case mKey of
    Just presented -> callerFromKey required presented
    Nothing -> do
      userId <- getRequestUserId
      pure $ Caller userId ApiKey.allScopes Nothing

{- | Resolve a presented key, refusing it for every reason it might be refused,
and charge the request against its rate limit.

Looked up by digest, so the key itself is never stored and a dump of this table
does not yield anyone's credentials.
-}
callerFromKey :: [ApiKey.Scope] -> Text -> Handler Caller
callerFromKey required presented = do
  now <- liftIO getCurrentTime
  mRow <- runDB $ getBy $ UniqueApiKeyDigest (ApiKey.digestOf presented)
  case mRow of
    Nothing -> notAuthenticated
    Just (Entity keyId key) -> do
      when (isJust $ arkhamApiKeyRevokedAt key) $ permissionDenied "This API key has been revoked"
      for_ (arkhamApiKeyExpiresAt key) \expiry ->
        when (expiry <= now) $ permissionDenied "This API key has expired"

      let granted = ApiKey.parseScopes (arkhamApiKeyScopes key)
          missing = filter (`notElem` granted) required
      unless (null missing) do
        permissionDenied
          $ "This API key is missing the scope(s): "
          <> ApiKey.renderScopes missing
          <> ". It has: "
          <> ApiKey.renderScopes granted

      -- Checked on every request, like revocation above, rather than only where
      -- a key is minted: taking the feature back off an account has to stop the
      -- keys it already made, or the rollout has no way back.
      owner <- runDB $ get404 (arkhamApiKeyUserId key)
      unless (owner.admin || Feature.hasFeature Feature.apiKeys owner.features)
        $ permissionDenied "API keys are not enabled for this account"

      -- A write is what is worth capping; a read costs a query and is not a way
      -- to fill a disk.
      let isWrite = ApiKey.cardsWrite `elem` required
      runDB $ chargeApiKey now keyId key isWrite

      pure $ Caller (arkhamApiKeyUserId key) granted (Just keyId)

{- | Note the key as used, and count the write against a fixed hourly window.

On the row rather than in the process: in the app a counter could only ever be
per-replica, and a limit that a second replica doubles is not a limit. It also
holds for someone calling the API directly instead of through the MCP server,
which is the case a limiter in front of the MCP server cannot see.
-}
chargeApiKey :: UTCTime -> ArkhamApiKeyId -> ArkhamApiKey -> Bool -> SqlPersistT Handler ()
chargeApiKey now keyId key isWrite = do
  let
    windowStart = arkhamApiKeyUsageWindowStart key
    withinWindow = maybe False (\started -> diffUTCTime now started < 3600) windowStart
    used = if withinWindow then arkhamApiKeyUsageCount key else 0
  when (isWrite && used >= ApiKey.writeLimitPerHour) do
    lift
      $ sendResponseStatus status429
      $ object
        [ "message"
            .= ( "This API key has made "
                   <> tshow ApiKey.writeLimitPerHour
                   <> " writes in the last hour, which is its limit. It will reset within the hour."
               )
        ]
  update keyId
    $ [ArkhamApiKeyLastUsedAt =. Just now]
    <> [ field
       | isWrite
       , field <-
           [ ArkhamApiKeyUsageCount =. used + 1
           , ArkhamApiKeyUsageWindowStart =. Just (if withinWindow then fromMaybe now windowStart else now)
           ]
       ]

{- | Refuse every request with a 503 while maintenance mode is on.

Composed OUTSIDE 'bugsnagYesodMiddleware' on purpose: a planned 503 is not an
error report, and six replicas refusing traffic for an hour would bury the real
exceptions under tens of thousands of them.

The allow-list is not a convenience, it is what keeps the site reachable at
all. All three Kubernetes probes AND the DigitalOcean load balancer health
check point at @/health@ (@terraform/app.tf@, @terraform/ingress.tf@), so a
@/health@ that 503s gets every container restarted and every node pulled out
of rotation -- taking the maintenance notice down along with the app, and
making the switch unreachable so it cannot be turned back off.
'ApiV1MaintenanceR' is allowed through because it IS the off switch, and
'ApiV1SiteSettingsR' because it is what tells the client to render the notice
instead of a board that silently fails to load.

In-flight requests are not interrupted, only new ones refused -- which is what
makes turning the mode on a drain rather than a kill. Whatever is mid-action
commits, the pool goes idle, and @pgPoolIdleTimeout@ (60s, "Settings") closes
the connections on its own. Note what that does NOT tell you: the app going
quiet is not the database being quiet. @pg_stat_activity@ is the only thing
that can say whether it is safe to take a lock.
-}
maintenanceMiddleware :: ToTypedContent res => Handler res -> Handler res
maintenanceMiddleware handler = do
  mMode <- getsYesod appMaintenance >>= readTVarIO
  mRoute <- getCurrentRoute
  case mMode of
    Just mode | not (any alwaysServed mRoute) -> do
      -- Long enough that a client backing off does not hammer six replicas for
      -- the length of the window, short enough to come back on its own after.
      addHeader "Retry-After" "120"
      sendResponseStatus status503
        $ object
          [ "maintenance" .= True
          , "since" .= maintenanceSince mode
          , "message" .= fromMaybe defaultMaintenanceMessage (maintenanceMessage mode)
          ]
    _ -> handler
 where
  alwaysServed = \case
    HealthR -> True
    ApiP (ApiV1P ApiV1MaintenanceR) -> True
    ApiP (ApiV1P ApiV1SiteSettingsR) -> True
    _ -> False

getAdminUser :: Handler (Entity User)
getAdminUser = do
  userId <- getRequestUserId
  user <- runDB $ get404 userId
  unless user.admin $ permissionDenied "You must be an admin to access this endpoint"
  pure $ Entity userId user

{- | The account behind this request, provided it has been let into a feature.

Admins pass whatever their row says: they are the ones doing the granting, and a
rollout whose first casualty is the person running it is not a rollout. Everyone
else needs the grant written on their row, which only 'ApiV1AdminUserFeaturesR'
does -- so the gate cannot be opened by the account it gates.
-}
getFeatureUser :: Feature.Feature -> Handler (Entity User)
getFeatureUser feature = do
  userId <- getRequestUserId
  user <- runDB $ get404 userId
  unless (user.admin || Feature.hasFeature feature user.features)
    $ permissionDenied "That feature is not enabled for your account"
  pure $ Entity userId user

getRequestUser :: Handler (Entity User)
getRequestUser = do
  userId <- getRequestUserId
  runDB $ getEntity404 userId

getEntity404
  :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend) => Key record -> DB (Entity record)
getEntity404 key = Entity key <$> get404 key
