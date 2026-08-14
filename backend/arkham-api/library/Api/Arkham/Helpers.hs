{-# LANGUAGE OverloadedRecordDot #-}

module Api.Arkham.Helpers where

import Arkham.Card
import Arkham.Classes hiding (Entity (..), select)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Debug
import Arkham.Epic.Types (EpicEnv, HasMaybeEpic (..), SharedEventState)
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.Queue
import Arkham.Random
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO, try)
import Control.Lens hiding (from)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Random (MonadRandom (..), StdGen)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID qualified as UUID
import Database.Esqueleto.Experimental
import Database.Redis (
  Connection,
  PubSubController,
  RedisChannel,
  addChannels,
  hdel,
  hgetall,
  hincrby,
  hset,
  pubSubForever,
  publish,
  runRedis,
 )
import Entity.Arkham.Game
import Entity.Arkham.LogEntry
import GHC.Records
import Import hiding (appLogger, (==.), (>=.))
import UnliftIO.Async qualified as UA

newtype GameLog = GameLog {gameLogToLogEntries :: [Text]}
  deriving newtype (Monoid, Semigroup)

instance HasField "entries" GameLog [Text] where
  getField = gameLogToLogEntries

newLogEntry :: ArkhamGameId -> Int -> UTCTime -> Text -> ArkhamLogEntry
newLogEntry gameId step now body =
  ArkhamLogEntry
    { arkhamLogEntryBody = body
    , arkhamLogEntryArkhamGameId = gameId
    , arkhamLogEntryStep = step
    , arkhamLogEntryCreatedAt = now
    }

getGameLog :: ArkhamGameId -> Maybe Int -> DB GameLog
getGameLog gameId mStep = fmap (GameLog . fmap unValue) $ select $ do
  entries <- from $ table @ArkhamLogEntry
  where_ $ entries.arkhamGameId ==. val gameId
  for_ mStep \step ->
    where_ $ entries.step >=. val step
  -- Order by step (monotonic per game) so the planner can use
  -- idx_arkham_log_entry_gameid_step directly without a Sort node.
  orderBy [asc entries.step, asc entries.id]
  pure entries.body

toPublicGame :: Entity ArkhamGame -> GameLog -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) gameLog =
  PublicGame gId arkhamGameName (gameLogToLogEntries gameLog) arkhamGameCurrentData

data ApiResponse
  = GameUpdate (PublicGame ArkhamGameId)
  | GameMessage Text
  | GameError Text
  | GameUI Text
  | GameAudio Text
  | GameCard {title :: Text, card :: Aeson.Value}
  | GameCardOnly {player :: PlayerId, title :: Text, card :: Aeson.Value}
  | GameTarot Aeson.Value
  | GameShowDiscard InvestigatorId
  | GameShowUnder InvestigatorId
  | {- | Above-the-table achievement unlocked (flat achievement tag); the
    client renders the toast from its i18n catalog.
    -}
    GameAchievement Text
  | GamePlayabilityInfo {cardId :: CardId, cardCode :: Text, checks :: [(Text, Maybe Text)]}
  | -- Epic Multiplayer: the event's shared state, pushed to a group's own stream
    -- so the shared panel renders from a single source (the group websocket).
    SharedStateUpdate SharedEventState
  | -- Event membership/group roster changed. Payloads are user-specific, so
    -- clients refetch EventDetails rather than receiving a shared digest.
    EventChanged
  deriving stock Generic

instance Aeson.ToJSON ApiResponse where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype
    ( MonadReader GameApp
    , Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadRandom
    , MonadMask
    , MonadCatch
    , MonadThrow
    , MonadUnliftIO
    )

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: Queue Message
  , appGen :: IORef StdGen
  , appLogger :: ClientMessage -> IO ()
  , appEvent :: Maybe EpicEnv
  {- ^ present only when this game is a group within an Epic Multiplayer event;
  'Nothing' (the default for every ordinary game) means zero behavior change.
  -}
  }

instance HasMaybeEpic GameApp where
  getMaybeEpicEnv = appEvent

instance HasDebugLevel GameAppT where
  getDebugLevel = liftIO getDebugLevel

instance HasGame GameAppT where
  getGame = readIORef =<< asks appGame
  getCache = GameCache \_ build -> build

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m {appGen = x}

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m {appGame = x}

instance HasQueue Message GameAppT where
  messageQueue = asks appQueue

instance HasGameLogger GameAppT where
  getLogger = do
    logger <- asks appLogger
    pure $ \msg -> liftIO $ logger msg

gameIdToText :: ArkhamGameId -> Text
gameIdToText = UUID.toText . coerce

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

gameChannel :: ArkhamGameId -> RedisChannel
gameChannel gameId = "arkham-" <> encodeUtf8 (tshow gameId)

{- | Epic Multiplayer: per-event broadcast channel + room helpers, mirroring the
per-game ones above but keyed by 'ArkhamEpicEventId' on 'appEventRooms'.
-}
eventChannel :: ArkhamEpicEventId -> RedisChannel
eventChannel eventId = "arkham-epic-" <> encodeUtf8 (tshow eventId)

{- | Join a room: look it up (creating it, and its one Redis subscription, when
this pod isn't serving the channel yet) AND register this WebSocket as a
subscriber, both in one turn of the rooms lock.

There is exactly ONE subscription per channel per pod and the room owns it;
every WebSocket on the room is then fed from that single callback. Each
connection used to open its own Redis connection and subscribe independently,
which meant every published message was delivered once per connection to
every connection (N^2 sends for an N-player table) and cost one Redis
connection per open browser tab.

Look-up and subscribe-to-room have to happen together. 'releaseRoomIfEmpty'
reads "present in the map with zero subscribers" as "nobody is here", so if a
connection could hold a room it had not yet registered on, a departing
connection could release that room out from under it -- leaving the new
arrival attached to a room whose channel is no longer subscribed, silently
receiving nothing for the life of the socket. Joining atomically is what makes
the zero-subscriber reading trustworthy.

'addChannels' only enqueues the SUBSCRIBE onto the controller's queue rather
than waiting for the ack, which keeps a Redis round-trip (or a controller
stalled mid-reconnect) from blocking every other connect and disconnect on the
pod.
-}
joinRoomIn
  :: (MonadIO m, HasApp m, Ord k)
  => (App -> MVar (Map k Room))
  -> (k -> RedisChannel)
  -> k
  -> m (Room, Int, Subscriber)
joinRoomIn roomsOf channelFor key = do
  app <- getApp
  liftIO $ modifyMVar (roomsOf app) \rooms -> do
    (rooms', r) <- ensureRoom app channelFor key rooms
    (subId, sub) <- subscribeToRoom r
    pure (rooms', (r, subId, sub))

-- | Look up or create a room. Assumes the caller holds the rooms 'MVar'.
ensureRoom
  :: Ord k
  => App -> (k -> RedisChannel) -> k -> Map k Room -> IO (Map k Room, Room)
ensureRoom app channelFor key rooms = case Map.lookup key rooms of
  Just r -> pure (rooms, r)
  Nothing -> do
    let chn = channelFor key
    r <- newRoom chn
    -- Real traffic counts as proof of life too, so a busy pod never trips
    -- the stall watchdog even if a heartbeat publish happens to fail.
    let onMessage m = do
          markPubSubAlive (appPubSubHealth app)
          broadcastToRoom r (BSL.fromStrict m)
    unsub <- case appMessageBroker app of
      WebSocketBroker -> pure (pure ())
      RedisBroker _ ctrl -> addChannels ctrl [(chn, onMessage)] []
    atomically $ writeTVar (roomUnsubscribe r) unsub
    pure (Map.insert key r rooms, r)

{- | Drop a room once its last WebSocket subscriber has left: remove it from the
map and tear down its Redis subscription, both under the rooms 'MVar'.

The emptiness check belongs under that lock too. Checking first and deleting
afterwards leaves a window in which a new connection joins the still-present
room and is then left holding one whose channel we immediately unsubscribe --
so it silently never receives another update. Returns whether the room was
actually released.
-}
releaseRoomIfEmpty
  :: (MonadIO m, HasApp m, Ord k) => (App -> MVar (Map k Room)) -> k -> m Bool
releaseRoomIfEmpty roomsOf key = do
  app <- getApp
  liftIO $ modifyMVar (roomsOf app) \rooms -> case Map.lookup key rooms of
    Nothing -> pure (rooms, False)
    Just r -> do
      n <- roomClientCount r
      if n > 0
        then pure (rooms, False)
        else do
          tryRedis_ $ join $ readTVarIO (roomUnsubscribe r)
          pure (Map.delete key rooms, True)

joinEventRoom
  :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m (Room, Int, Subscriber)
joinEventRoom = joinRoomIn appEventRooms eventChannel

releaseEventRoomIfEmpty :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m Bool
releaseEventRoomIfEmpty = releaseRoomIfEmpty appEventRooms

lookupEventRoom :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m (Maybe Room)
lookupEventRoom eid = do
  roomsVar <- getsApp appEventRooms
  Map.lookup eid <$> liftIO (MVar.readMVar roomsVar)

joinGameRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m (Room, Int, Subscriber)
joinGameRoom = joinRoomIn appGameRooms gameChannel

releaseGameRoomIfEmpty :: (MonadIO m, HasApp m) => ArkhamGameId -> m Bool
releaseGameRoomIfEmpty = releaseRoomIfEmpty appGameRooms

{- | Like 'joinGameRoom' but never creates a Room and never subscribes. Use
from the publish path so that broadcasting to a game with no listeners
doesn't leak an empty Room into 'appGameRooms' that nothing will ever
clean up.
-}
lookupRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m (Maybe Room)
lookupRoom gid = do
  roomsVar <- getsApp appGameRooms
  Map.lookup gid <$> liftIO (MVar.readMVar roomsVar)

{- | Cross-server room registry. Two parallel Redis hashes:

  * 'roomsHashKey'      gameId -> total WebSocket subscribers
  * 'roomsSeenHashKey'  gameId -> unix epoch of last activity

Every subscribe/unsubscribe and a per-pod heartbeat refresh the seen
timestamp. On admin read, entries whose seen timestamp is older than
'roomStaleSeconds' (or missing entirely) are treated as crashed-pod
cruft, removed from both hashes, and excluded from the response.
-}
roomsHashKey :: ByteString
roomsHashKey = "arkham:rooms"

roomsSeenHashKey :: ByteString
roomsSeenHashKey = "arkham:rooms:seen"

-- A room without an updated seen timestamp for this many seconds is
-- treated as stale. Sized to be 3x the heartbeat cadence so a single
-- missed write doesn't drop a live game.
roomStaleSeconds :: Int
roomStaleSeconds = 90

-- Cadence for 'roomHeartbeat' (the per-pod "I'm still serving these
-- games" pulse).
roomHeartbeatSeconds :: Int
roomHeartbeatSeconds = 30

roomField :: ArkhamGameId -> ByteString
roomField = encodeUtf8 . gameIdToText

currentEpoch :: IO Int
currentEpoch = floor <$> getPOSIXTime

-- Best-effort wrapper: tracking room counts is observability, not
-- correctness, so we never let a Redis hiccup tear down a live session.
tryRedis_ :: MonadIO m => IO a -> m ()
tryRedis_ action = void $ liftIO $ try @SomeException action

-- Run a best-effort Redis action if a Redis broker is configured.
withRedis :: (MonadIO m, HasApp m) => (Connection -> IO a) -> m ()
withRedis action = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure ()
    RedisBroker conn _ -> tryRedis_ (action conn)

{- | Increment the cross-server client count for a game in Redis and
refresh its seen timestamp. No-op without a Redis broker.
-}
incrRoomMember :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
incrRoomMember gameId = withRedis \conn -> runRedis conn do
  void $ hincrby roomsHashKey (roomField gameId) 1
  now <- liftIO currentEpoch
  void $ hset roomsSeenHashKey ((roomField gameId, BS8.pack (show now)) :| [])

{- | Decrement the cross-server client count. When the new count is at or
below zero, drop the field from both hashes so the admin view doesn't
carry empty rooms; otherwise refresh the seen timestamp.
-}
decrRoomMember :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
decrRoomMember gameId = withRedis \conn -> runRedis conn do
  result <- hincrby roomsHashKey (roomField gameId) (-1)
  case result of
    Right n | n <= 0 -> do
      void $ hdel roomsHashKey (roomField gameId :| [])
      void $ hdel roomsSeenHashKey (roomField gameId :| [])
    _ -> do
      now <- liftIO currentEpoch
      void $ hset roomsSeenHashKey ((roomField gameId, BS8.pack (show now)) :| [])

{- | Aggregate client counts across servers from Redis, filtering out
entries whose 'seen' timestamp is older than 'roomStaleSeconds'. Stale
entries are also HDEL'd from both hashes so cruft from crashed pods
doesn't accumulate. Returns 'Nothing' when no Redis broker is
configured (callers fall back to local state).
-}
getRedisRoomCounts :: (MonadIO m, HasApp m) => m (Maybe (Map ArkhamGameId Int))
getRedisRoomCounts = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure Nothing
    RedisBroker conn _ -> do
      now <- liftIO currentEpoch
      result <- liftIO $ try @SomeException $ runRedis conn do
        countsR <- hgetall roomsHashKey
        seenR <- hgetall roomsSeenHashKey
        pure (countsR, seenR)
      case result of
        Right (Right counts, Right seen) -> do
          let countMap = Map.fromList (mapMaybe parseIntEntry counts)
              seenMap = Map.fromList (mapMaybe parseIntEntry seen)
              isFresh gid = case Map.lookup gid seenMap of
                Just t -> now - t < roomStaleSeconds
                Nothing -> False
              (fresh, stale) = Map.partitionWithKey (\gid _ -> isFresh gid) countMap
          unless (Map.null stale)
            $ tryRedis_
            $ sweepStaleRooms conn (Map.keys stale)
          pure $ Just fresh
        _ -> pure (Just Map.empty)
 where
  parseIntEntry (k, v) = do
    uuid <- UUID.fromText (decodeUtf8 k)
    n <- readMaybe (BS8.unpack v)
    pure (coerce uuid :: ArkhamGameId, n)

sweepStaleRooms :: Connection -> [ArkhamGameId] -> IO ()
sweepStaleRooms conn gameIds = for_ (nonEmpty $ map roomField gameIds) \fields ->
  void $ runRedis conn do
    void $ hdel roomsHashKey fields
    void $ hdel roomsSeenHashKey fields

{- | Background heartbeat: every 'roomHeartbeatSeconds' refresh the seen
timestamp for every game this pod still has live subscribers for. This
keeps active games out of the staleness sweep even when nothing else
(subscribe / unsubscribe) is writing to Redis. Run once per pod via
'forkIO' from 'makeFoundation'.
-}
roomHeartbeat :: App -> IO ()
roomHeartbeat app = case appMessageBroker app of
  WebSocketBroker -> pure ()
  RedisBroker conn _ -> forever do
    threadDelay (roomHeartbeatSeconds * 1000000)
    rooms <- MVar.readMVar (appGameRooms app)
    active <- catMaybes <$> traverse keepIfActive (Map.toList rooms)
    unless (null active) do
      now <- currentEpoch
      void $ try @SomeException $ runRedis conn do
        for_ active \gid ->
          void $ hset roomsSeenHashKey ((roomField gid, BS8.pack (show now)) :| [])
 where
  keepIfActive (gid, room) = do
    n <- roomClientCount room
    pure $ if n > 0 then Just gid else Nothing

{- | Channel every pod subscribes to and publishes a heartbeat on, purely to
prove the pub/sub path is alive end to end.

This is the one subscription that is never removed: it is registered as an
initial subscription on the 'PubSubController', so 'pubSubForever' restores
it on every reconnect and the controller never sits at zero channels.
-}
pubSubHealthChannel :: RedisChannel
pubSubHealthChannel = "arkham:pubsub:health"

-- How often each pod publishes a pub/sub heartbeat.
pubSubHeartbeatSeconds :: Int
pubSubHeartbeatSeconds = 20

{- | How long the subscriber socket may go without delivering anything before
we call it dead and reconnect. Three missed beats, so a slow Redis or a GC
pause can't trip it. Every pod publishes and every pod is subscribed, so a
healthy pod sees a beat every 'pubSubHeartbeatSeconds' regardless of how
quiet the games themselves are.
-}
pubSubStaleSeconds :: NominalDiffTime
pubSubStaleSeconds = 70

-- Cap on the reconnect backoff between 'pubSubForever' attempts.
pubSubMaxBackoffSeconds :: Int
pubSubMaxBackoffSeconds = 30

-- An attempt that survived this long is treated as healthy, resetting backoff.
pubSubHealthyRunSeconds :: NominalDiffTime
pubSubHealthyRunSeconds = 120

-- | Record that the subscriber socket just delivered something.
markPubSubAlive :: MonadIO m => TVar UTCTime -> m ()
markPubSubAlive healthVar = liftIO do
  now <- getCurrentTime
  atomically $ writeTVar healthVar now

data PubSubStalled = PubSubStalled
  deriving stock Show
  deriving anyclass Exception

{- | Own this pod's single pub/sub subscriber connection.

Two distinct failure modes, and 'pubSubForever' alone survives neither:

* It throws on network death and has to be re-called to resubscribe every
  channel tracked by the controller (hedis documents exactly this). It used
  to be forked bare, so a single Redis blip would have silently ended all
  cross-pod delivery for the remaining life of the pod.

* It does not notice a HALF-OPEN socket. An idle TCP connection dropped by a
  proxy leaves it blocked in @recv@ forever -- still "connected", delivering
  nothing, throwing nothing. That is what strands a quiet game: the WebSocket
  stays healthy (its own ping thread keeps it up) and log lines keep flowing,
  because those are broadcast in-process, while every GameUpdate -- which
  travels via Redis -- is silently lost.

So each pod publishes to 'pubSubHealthChannel' over the ordinary connection
pool, meaning the beat travels the exact route a GameUpdate does, and every
pod is subscribed to it. If nothing at all arrives on the subscriber socket
for 'pubSubStaleSeconds', we tear the connection down and reconnect. The beat
doubles as keepalive traffic, so in the common case the idle drop never
happens in the first place.
-}
pubSubSupervisor :: TVar UTCTime -> Connection -> PubSubController -> IO ()
pubSubSupervisor healthVar conn ctrl = go 1
 where
  go backoff = do
    markPubSubAlive healthVar
    startedAt <- getCurrentTime
    outcome <-
      try @SomeException
        $ UA.race_ (pubSubForever conn ctrl (markPubSubAlive healthVar)) watchdog
    endedAt <- getCurrentTime
    putStrLn $ case outcome of
      Left e -> "pubsub subscriber died, reconnecting: " <> show e
      Right () -> "pubsub subscriber stalled, reconnecting"
    threadDelay (backoff * 1000000)
    go
      $ if diffUTCTime endedAt startedAt > pubSubHealthyRunSeconds
        then 1
        else min pubSubMaxBackoffSeconds (backoff * 2)

  watchdog = forever do
    threadDelay (pubSubHeartbeatSeconds * 1000000)
    tryRedis_ $ runRedis conn $ publish pubSubHealthChannel "ping"
    now <- getCurrentTime
    seen <- readTVarIO healthVar
    when (diffUTCTime now seen > pubSubStaleSeconds) $ throwIO PubSubStalled

lockGame :: ArkhamGameId -> DB ()
lockGame gameId = void $ select do
  game <- from $ table @ArkhamGame
  where_ $ game.id ==. val gameId
  locking forUpdate

{- | One round-trip in the hot path: lock the row AND fetch its data.
Replaces the previous lockGame + get404 pair, halving the DB calls
for every caller of atomicallyWithGame on the success path.
(notFound lives in MonadHandler, but DB is rank-2 over MonadIO; on the
rare missing-game path we delegate to get404 to throw the 404, which
costs one extra empty SELECT only when the game doesn't exist.)
-}
atomicallyWithGame :: ArkhamGameId -> (ArkhamGame -> DB a) -> DB a
atomicallyWithGame gameId f = do
  results <- select do
    game <- from $ table @ArkhamGame
    where_ $ game.id ==. val gameId
    locking forUpdate
    pure game
  case results of
    [] -> do
      game <- get404 gameId
      f game
    (Entity _ game : _) -> f game
