{-# LANGUAGE OverloadedRecordDot #-}

module Api.Arkham.Helpers where

import Api.Arkham.Maintenance
import Api.Arkham.RoomOps
import Arkham.Card
import Arkham.Classes hiding (Entity (..), select)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Debug
import Arkham.Epic.Types (EpicEnv, HasMaybeEpic (..), SharedEventState)
import Arkham.Game
import Arkham.Id
import Arkham.Log.Entry
import Arkham.Message
import Arkham.Phase qualified as Phase
import Arkham.Queue
import Arkham.Random
import Control.Concurrent (forkIO, threadDelay)
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
import Data.Set qualified as Set
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.Esqueleto.Experimental

-- Aliased for '<.', which Control.Lens and persistent also define.
import Database.Esqueleto.Experimental qualified as E
import Database.Redis (
  Connection,
  PubSubController,
  RedisChannel,
  addChannels,
  hdel,
  hget,
  hgetall,
  hincrby,
  hset,
  pubSubForever,
  publish,
  runRedis,
 )
import Database.Redis qualified as Redis
import Entity.Arkham.Game
import Entity.Arkham.LogEntry
import GHC.Records
import Import hiding (appLogger, (==.), (>=.))
import UnliftIO.Async qualified as UA

newtype GameLog = GameLog {gameLogToLogEntries :: [LogRow]}
  deriving newtype (Monoid, Semigroup)

instance HasField "entries" GameLog [LogRow] where
  getField = gameLogToLogEntries

{- | How much log the game payload carries.

The client renders a window of the most recent entries and pages scrollback on
demand, so shipping the whole history on every fetch was pure waste: 97.9% of
the log bytes in a measured mid-campaign payload were discarded immediately.
Forty is comfortably more than fits the panel, so a reader can scroll a little
without a round trip.
-}
gameLogTailSize :: Int
gameLogTailSize = 40

{- | A persisted row as the client should see it.

A row written since the overhaul has a @payload@ and is handed over as-is. An
older row has only its flat @body@, which the client parses once at ingest --
see 'Arkham.Log.Entry.LogRow'. A payload that fails to decode falls back to the
legacy shape rather than vanishing: it is someone's game history, and the flat
body is always present.

Either way the row's own @step@ column wins over anything in the payload: it is
the column an undo actually targets, and it is present on every row ever
written, including the whole history that predates the field.
-}
toLogRow :: ArkhamLogEntry -> LogRow
toLogRow e = case arkhamLogEntryPayload e of
  Just v | Aeson.Success entry <- Aeson.fromJSON v -> LogRowStructured (atLogStep step entry)
  _ -> LogRowLegacy (arkhamLogEntryBody e) (Just step)
 where
  step = arkhamLogEntryStep e

{- | A legacy, text-only row: the brace-DSL string and nothing else.

Still used by every 'Arkham.Classes.GameLogger.ClientText' that has not been
migrated to a structured entry. Rows written this way have a NULL @payload@ and
the client parses their @body@ once at ingest.
-}
newLogEntry :: ArkhamGameId -> Int -> UTCTime -> Text -> ArkhamLogEntry
newLogEntry gameId step now body =
  ArkhamLogEntry
    { arkhamLogEntryBody = body
    , arkhamLogEntryPayload = Nothing
    , arkhamLogEntrySeq = Nothing
    , arkhamLogEntryGroupId = Nothing
    , arkhamLogEntryArkhamGameId = gameId
    , arkhamLogEntryStep = step
    , arkhamLogEntryCreatedAt = now
    }

{- | A structured row.

@body@ is still filled, with 'logEntryToText', for two reasons: the legacy
@PublicGame@ log is a @[Text]@ and clients that have not switched keep working,
and a flat rendering is what the replay trace and any server-side error message
can use. It is lossy -- refs become their names, an i18n key becomes the key --
so nothing should read it back where @payload@ is available.
-}
newStructuredLogEntry :: ArkhamGameId -> Int -> UTCTime -> LogEntry -> ArkhamLogEntry
newStructuredLogEntry gameId step now entry =
  ArkhamLogEntry
    { arkhamLogEntryBody = logEntryToText entry
    , arkhamLogEntryPayload = Just (Aeson.toJSON entry)
    , arkhamLogEntrySeq = Just entry.logEntrySeq
    , arkhamLogEntryGroupId = logGroupId <$> entry.logEntryGroup
    , arkhamLogEntryArkhamGameId = gameId
    , arkhamLogEntryStep = step
    , arkhamLogEntryCreatedAt = now
    }

{- | The next sequence number for a game's structured log entries.

Legacy text rows have a NULL seq and are skipped by @max@, so a game part-way
through the migration numbers its structured entries 1, 2, 3... regardless of
how much flat history sits in front of them. Indexed by
@idx_arkham_log_entry_gameid_seq@.
-}
nextLogSeq :: ArkhamGameId -> DB Int
nextLogSeq gameId = do
  rows <- select do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
    -- `seq` is nullable (legacy rows have none), so max_ comes back doubly
    -- wrapped; joinV collapses it.
    pure $ joinV $ max_ entries.seq
  pure $ case rows of
    (Value (Just n) : _) -> n + 1
    _ -> 1

{- | The whole log for a game. Only the admin view wants this; everything on the
request path uses 'getGameLogTail'.
-}
getGameLog :: ArkhamGameId -> Maybe Int -> DB GameLog
getGameLog gameId mStep = fmap (GameLog . map (toLogRow . entityVal)) $ select $ do
  entries <- from $ table @ArkhamLogEntry
  where_ $ entries.arkhamGameId ==. val gameId
  for_ mStep \step ->
    where_ $ entries.step >=. val step
  -- Order by step (monotonic per game) so the planner can use
  -- idx_arkham_log_entry_gameid_step directly without a Sort node.
  orderBy [asc entries.step, asc entries.id]
  pure entries

{- | The most recent @n@ rows, oldest first.

Reads descending so the index serves the LIMIT, then reverses the (small) page
in Haskell rather than making Postgres sort the whole history.
-}
getGameLogTail :: ArkhamGameId -> Int -> DB GameLog
getGameLogTail gameId n = do
  rows <- select $ do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
    orderBy [desc entries.step, desc entries.id]
    limit (fromIntegral n)
    pure entries
  pure $ GameLog $ reverse $ map (toLogRow . entityVal) rows

{- | The page of rows immediately older than @before@, oldest first.

Scrollback. The payload only ever carries 'gameLogTailSize' rows, so without
this the rest of a game's history is durable but unreachable in play.

Paged on @seq@, which is monotonic per game and is the client's own identity for
a row, so a page is exact: no offset to drift when new entries land at the other
end while the reader is scrolling back.

__Structured rows only.__ Rows written before the overhaul have a NULL @seq@ and
are strictly older than every structured one, so they sort before this window
and this cannot reach them. They are a transitional artifact and will age out of
any game still being played; the alternative is a second, messier cursor over
@(step, id)@ for history nobody is adding to.
-}
getGameLogBefore :: ArkhamGameId -> Int -> Int -> DB GameLog
getGameLogBefore gameId before n = do
  rows <- select $ do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
    where_ $ entries.seq E.<. just (val before)
    -- By seq, not (step, id): a retraction can leave a gap, and an undo can
    -- move a chat row's step, so seq is the only monotonic thing here.
    orderBy [desc entries.seq]
    limit (fromIntegral n)
    pure entries
  pure $ GameLog $ reverse $ map (toLogRow . entityVal) rows

toPublicGame :: Entity ArkhamGame -> GameLog -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) gameLog =
  PublicGame gId arkhamGameName (gameLogToLogEntries gameLog) arkhamGameCurrentData

data ApiResponse
  = GameUpdate (PublicGame ArkhamGameId)
  | PhaseChanged Phase.Phase
  | GameMessage Text
  | GameError Text
  | GameUI Text
  | GameAudio Text
  | GameCard {title :: Text, card :: Aeson.Value}
  | GameCardOnly {player :: PlayerId, title :: Text, card :: Aeson.Value}
  | {- | One investigator drew cards from their own deck. Carries the whole draw
    rather than one message per card, so a six-card draw is one thing to look at
    instead of six. Whether it is shown at all, and how, is the receiving
    client's business -- it is a display preference, not game state.
    -}
    GameDrewCards {player :: PlayerId, title :: Text, cards :: Aeson.Value, kind :: Text}
  | GameTarot Aeson.Value
  | GameShowDiscard InvestigatorId
  | GameShowUnder InvestigatorId
  | {- | Above-the-table achievement unlocked (flat achievement tag); the
    client renders the toast from its i18n catalog.
    -}
    GameAchievement Text
  | GamePlayabilityInfo {cardId :: CardId, cardCode :: Text, checks :: [(Text, Maybe Text)]}
  | {- | A custom card's JSON could not be used. Sent to the table rather than
    swallowed, with the fragment at fault, so its author can go and fix it.
    -}
    GameCustomCardIssue {cardCode :: Text, detail :: Text, payload :: Aeson.Value}
  | -- Epic Multiplayer: the event's shared state, pushed to a group's own stream
    -- so the shared panel renders from a single source (the group websocket).
    SharedStateUpdate SharedEventState
  | -- Event membership/group roster changed. Payloads are user-specific, so
    -- clients refetch EventDetails rather than receiving a shared digest.
    EventChanged
  | {- | A game's undo history came back from cold storage ("Api.Arkham.Archive").

    The board does not change -- the steps behind it do -- so this exists to tell
    the client that pressed Unarchive that it may stop waiting, and to tell
    anyone else at the table why undo suddenly works again.
    -}
    GameUnarchived
  | -- | The restore could not be completed; the message says what happened.
    GameUnarchiveFailed Text
  | {- | How many people are watching this game without a seat in it.

    Pushed on every connect and disconnect rather than polled: the room already
    knows, and a table of four asking "is anyone watching?" on a timer is four
    requests a tick to usually hear "no". Sent to the whole room, watchers
    included, so everyone sees the same number.
    -}
    GameWatcherCount Int
  | {- | The room this socket is attached to has been deleted -- the game was
    deleted, or an admin dropped the room. The socket is torn down right behind
    this message, so it is the last thing the client receives; reconnecting
    would only recreate the room it was just removed from, which is why the
    client is expected to leave rather than retry.
    -}
    RoomClosed Text
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
  => SubscriberInfo
  -> (App -> MVar (Map k Room))
  -> (k -> RedisChannel)
  -> k
  -> m (Room, Int, Subscriber)
joinRoomIn joining roomsOf channelFor key = do
  app <- getApp
  liftIO $ modifyMVar (roomsOf app) \rooms -> do
    (rooms', r) <- ensureRoom app channelFor key rooms
    (subId, sub) <- subscribeToRoom joining r
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
joinEventRoom eid = do
  joining <- anonymousSubscriber
  joinRoomIn joining appEventRooms eventChannel eid

releaseEventRoomIfEmpty :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m Bool
releaseEventRoomIfEmpty = releaseRoomIfEmpty appEventRooms

lookupEventRoom :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m (Maybe Room)
lookupEventRoom eid = do
  roomsVar <- getsApp appEventRooms
  Map.lookup eid <$> liftIO (MVar.readMVar roomsVar)

joinGameRoom
  :: (MonadIO m, HasApp m)
  => SubscriberInfo -> ArkhamGameId -> m (Room, Int, Subscriber)
joinGameRoom joining = joinRoomIn joining appGameRooms gameChannel

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

{- | Publish to a game's room with nothing but the 'App'.

'Api.Handler.Arkham.Games.Shared.publishToRoom' needs 'HasApp', which only the
handler monad has; the archive workers run on 'forkIO' and hold the foundation
directly. Same two cases as that function, including dropping the message when
no Redis broker is configured and nobody is listening -- the next subscriber
reads the current state from the database anyway.
-}
publishFromApp :: Aeson.ToJSON a => App -> ArkhamGameId -> a -> IO ()
publishFromApp app gameId a = case appMessageBroker app of
  RedisBroker conn _ ->
    tryRedis_ $ runRedis conn $ publish (gameChannel gameId) (BSL.toStrict $ Aeson.encode a)
  WebSocketBroker -> do
    rooms <- MVar.readMVar (appGameRooms app)
    for_ (Map.lookup gameId rooms) \room -> broadcastToRoom room (Aeson.encode a)

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

{- | Cross-server spectator count: gameId -> sockets watching without a seat.

A third hash beside 'roomsHashKey' rather than a breakdown inside it, because
the two are counted on different events -- every socket moves the room count,
only a spectator's moves this one -- and because an older pod writing only the
room count then stays correct about what it does write. Shares
'roomsSeenHashKey' for freshness, so a crashed pod's watchers are swept by the
same pass that sweeps its rooms.

Best-effort, exactly as the room count beside it is: a pod that dies with
watchers attached leaves its share of the tally behind until that sweep, which
cannot run while another pod is keeping the game fresh. The number on the bar
is "who is watching", not a figure anything is decided on.
-}
watchersHashKey :: ByteString
watchersHashKey = "arkham:watchers"

-- A room without an updated seen timestamp for this many seconds is
-- treated as stale. Sized to be 3x the heartbeat cadence so a single
-- missed write doesn't drop a live game.
roomStaleSeconds :: Int
roomStaleSeconds = 90

-- Cadence for 'roomHeartbeat' (the per-pod "I'm still serving these
-- games" pulse).
roomHeartbeatSeconds :: Int
roomHeartbeatSeconds = 30

{- | How long a room may hold nothing but spectators before it is closed.

Comfortably longer than a reload, which is the one ordinary way a game ends up
with watchers and no players for a moment, and short enough that a watcher is
not left staring at a table everybody else has left. Measured from the first
sweep that saw no players, so the real wait is this plus up to one
'roomHeartbeatSeconds'.
-}
roomAbandonedSeconds :: Int
roomAbandonedSeconds = 60

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

{- | Write the cross-server tallies for a game from a count that was just
observed, rather than adjusting them by one.

The tallies are only ever moved by 'incrRoomMember' and 'decrRoomMember', so a
pod that dies with sockets attached never decrements and the count keeps its
share forever. Nothing reconciles that: 'getRedisRoomCounts' sweeps a game
whose @seen@ has gone stale, which never happens while another pod is still
holding the room and refreshing it. The drift then reads as seated players,
which is what stops the abandoned-room sweep closing the room -- a state that
sustains itself.

So an admin sweep that has just probed every socket can say what the number
actually is. Zero drops the fields, the same shape 'decrRoomMember' leaves
behind, so an empty room does not linger on the admin list.
-}
setRoomTallies :: (MonadIO m, HasApp m) => ArkhamGameId -> Int -> Int -> m ()
setRoomTallies gameId sockets watchers = withRedis \conn -> runRedis conn do
  if sockets <= 0
    then do
      void $ hdel roomsHashKey (roomField gameId :| [])
      void $ hdel roomsSeenHashKey (roomField gameId :| [])
    else do
      now <- liftIO currentEpoch
      void $ hset roomsHashKey ((roomField gameId, BS8.pack (show sockets)) :| [])
      void $ hset roomsSeenHashKey ((roomField gameId, BS8.pack (show now)) :| [])
  if watchers <= 0
    then void $ hdel watchersHashKey (roomField gameId :| [])
    else void $ hset watchersHashKey ((roomField gameId, BS8.pack (show watchers)) :| [])

{- | Increment the cross-server spectator count for a game. No-op without a
Redis broker, where the room's own tally is the whole truth.
-}
incrRoomWatcher :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
incrRoomWatcher gameId = withRedis \conn -> runRedis conn do
  void $ hincrby watchersHashKey (roomField gameId) 1

{- | Decrement the cross-server spectator count, dropping the field once the
last watcher has gone so an empty game carries no entry at all.
-}
decrRoomWatcher :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
decrRoomWatcher gameId = withRedis \conn -> runRedis conn do
  result <- hincrby watchersHashKey (roomField gameId) (-1)
  case result of
    Right n | n <= 0 -> void $ hdel watchersHashKey (roomField gameId :| [])
    _ -> pure ()

{- | The cross-server spectator count for one game.

'Nothing' when there is no Redis broker, or when the read failed -- in both
cases the caller falls back to the room's own tally, which is the right answer
on a single pod and a low one rather than a wrong one on several.
-}
getRedisWatcherCount :: (MonadIO m, HasApp m) => ArkhamGameId -> m (Maybe Int)
getRedisWatcherCount gameId = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure Nothing
    RedisBroker conn _ -> do
      result <- liftIO $ try @SomeException $ runRedis conn $ hget watchersHashKey (roomField gameId)
      pure case result of
        Right (Right (Just raw)) -> readMaybe (BS8.unpack raw)
        Right (Right Nothing) -> Just 0
        _ -> Nothing

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

{- | The games somebody is actually connected to right now.

Cross-pod through Redis when a broker is configured -- one @HGETALL@, already
swept of entries left behind by crashed pods -- and this pod's own room map
otherwise, which is the whole truth on a single pod and all a local dev setup
has.

A room with no clients does not count. A room outlives its last socket until
its owner releases it, and the question here is whether anyone is there, not
whether a room object exists.
-}
getLiveGameRooms :: (MonadIO m, HasApp m) => m (Set ArkhamGameId)
getLiveGameRooms =
  getRedisRoomCounts >>= \case
    Just counts -> pure $ Map.keysSet $ Map.filter (> 0) counts
    Nothing -> do
      roomsVar <- getsApp appGameRooms
      rooms <- liftIO $ MVar.readMVar roomsVar
      occupied <- liftIO $ filterM (fmap (> 0) . roomClientCount . snd) (Map.assocs rooms)
      pure $ Set.fromList (map fst occupied)

{- | The whole spectator hash, for the admin rooms list.

Not filtered for staleness: it shares 'roomsSeenHashKey' with the room counts,
which 'getRedisRoomCounts' has already swept by the time this is read, and a
watcher count for a game that is not in that list is not shown anyway.
-}
getRedisWatcherCounts :: (MonadIO m, HasApp m) => m (Map ArkhamGameId Int)
getRedisWatcherCounts = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure Map.empty
    RedisBroker conn _ -> do
      result <- liftIO $ try @SomeException $ runRedis conn $ hgetall watchersHashKey
      pure case result of
        Right (Right entries) -> Map.fromList (mapMaybe parseEntry entries)
        _ -> Map.empty
 where
  parseEntry (k, v) = do
    uuid <- UUID.fromText (decodeUtf8 k)
    n <- readMaybe (BS8.unpack v)
    pure (coerce uuid :: ArkhamGameId, n)

sweepStaleRooms :: Connection -> [ArkhamGameId] -> IO ()
sweepStaleRooms conn gameIds = for_ (nonEmpty $ map roomField gameIds) \fields ->
  void $ runRedis conn do
    void $ hdel roomsHashKey fields
    void $ hdel roomsSeenHashKey fields
    -- Counted on the same connects as the room itself, so it goes stale with it.
    void $ hdel watchersHashKey fields

{- | What Redis records about one game's room, as opposed to what the pods
holding it actually have open.

The two are allowed to disagree, and the disagreement is the diagnosis: a
'tallyClients' of 3 against a single socket on a single pod is two sockets'
worth of tally left behind by a replica that died holding them, and that
leftover is what 'seatedPlayerCount' reads as a seated player and
'sweepAbandonedRooms' therefore refuses to touch.
-}
data RoomTally = RoomTally
  { tallyClients :: Maybe Int
  , tallyWatchers :: Maybe Int
  , tallySeenAt :: Maybe UTCTime
  -- ^ When a pod last claimed to be serving this game.
  }
  deriving stock (Show, Generic)
  deriving anyclass Aeson.ToJSON

getRoomTally :: (MonadIO m, HasApp m) => ArkhamGameId -> m RoomTally
getRoomTally gameId = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure $ RoomTally Nothing Nothing Nothing
    RedisBroker conn _ -> do
      result <- liftIO $ try @SomeException $ runRedis conn do
        clients <- hget roomsHashKey (roomField gameId)
        watchers <- hget watchersHashKey (roomField gameId)
        seen <- hget roomsSeenHashKey (roomField gameId)
        pure (clients, watchers, seen)
      pure case result of
        Right (Right clients, Right watchers, Right seen) ->
          RoomTally
            { tallyClients = readInt =<< clients
            , tallyWatchers = readInt =<< watchers
            , tallySeenAt = fmap epochToUTC . readInt =<< seen
            }
        _ -> RoomTally Nothing Nothing Nothing
 where
  readInt :: ByteString -> Maybe Int
  readInt = readMaybe . BS8.unpack
  epochToUTC = posixSecondsToUTCTime . fromIntegral

{- | Forget a game's rows in all three room hashes.

The counts are maintained by increments and decrements from the sockets
themselves, which is exactly why they can be wrong: a pod that dies with
sockets attached never sends its decrements, and its share of the tally stays
behind. 'sweepStaleRooms' cleans that up only for a game nothing else is
keeping fresh -- a leftover player count on a game one spectator is still
watching is refreshed by that spectator's pod forever, reads as a seated
player to 'seatedPlayerCount', and so keeps the room out of
'sweepAbandonedRooms' for good.

That is the shape of an admin's stuck room, so closing one has to be able to
drop the tally as well as the sockets. The ordinary decrements stay correct
either way: 'decrRoomMember' deletes the field again whenever its count
reaches zero.
-}
purgeRoomTallies :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
purgeRoomTallies gameId = withRedis \conn -> runRedis conn do
  void $ hdel roomsHashKey (roomField gameId :| [])
  void $ hdel roomsSeenHashKey (roomField gameId :| [])
  void $ hdel watchersHashKey (roomField gameId :| [])

{- | Perform one room op against this pod's own rooms.

The same function on both sides of the Redis hop: it is what a pod runs when a
request arrives on 'roomOpsChannel', and what the handler runs directly when
there is no Redis broker at all, so development exercises the same code as
production with one pod in the fleet instead of six.
-}
runRoomOpOn :: Text -> MVar (Map ArkhamGameId Room) -> ArkhamGameId -> RoomOp -> IO RoomOpReply
runRoomOpOn podId roomsVar gameId roomOp = do
  rooms <- MVar.readMVar roomsVar
  case Map.lookup gameId rooms of
    Nothing -> pure $ RoomOpReply podId False False []
    Just room -> case roomOp of
      InspectRoom -> RoomOpReply podId True False <$> roomConnectionsOn podId room
      ProbeRoom ->
        RoomOpReply podId True False <$> probeRoomConnections podId roomProbeWaitMicros room
      CloseRoom reason -> do
        -- Read before closing: afterwards there is nothing left to describe,
        -- and who was evicted is the whole record of what the close did.
        conns <- roomConnectionsOn podId room
        closed <- closeRoomIn roomsVar gameId (Aeson.encode $ RoomClosed reason)
        pure $ RoomOpReply podId True closed conns

{- | Pub/sub handler for 'roomOpsChannel': answer for the room this pod holds.

Forked rather than run inline. The callback runs on the controller's single
subscriber thread, and a probe deliberately sits still for
'roomProbeWaitMicros' waiting for pongs -- doing that on that thread would
stall every cross-pod game update on the pod for two and a half seconds.

Best-effort like the rest of the room bookkeeping: a reply that cannot be
written is a pod missing from the admin's answer, which the answer says, and
never a failure that touches a live game.
-}
onRoomOpMessage
  :: Connection -> Text -> MVar (Map ArkhamGameId Room) -> ByteString -> IO ()
onRoomOpMessage conn podId roomsVar raw =
  for_ (Aeson.decodeStrict @RoomOpRequest raw) \req ->
    for_ (UUID.fromText req.roomOpGameId) \uuid -> void $ forkIO do
      reply <- runRoomOpOn podId roomsVar (coerce uuid) req.roomOpOp
      let key = roomOpsReplyKey req.roomOpId
      tryRedis_ $ runRedis conn do
        void $ Redis.rpush key (BSL.toStrict (Aeson.encode reply) :| [])
        void $ Redis.expire key roomOpsReplyTtlSeconds

{- | Run a room op on every pod and collect what they say.

Publish, wait a fixed window, read the reply list, delete it. A fixed window
and not a quorum because nothing here knows how many replicas are running: the
answer carries the pods that replied, and an admin reading "two pods answered,
neither held this room" is better served than one watching a request hang for
the pod that will never answer.

Without Redis this is a fleet of one and the op runs in place.
-}
runRoomOp :: (MonadIO m, HasApp m) => ArkhamGameId -> RoomOp -> m [RoomOpReply]
runRoomOp gameId roomOp = do
  app <- getApp
  case appMessageBroker app of
    WebSocketBroker ->
      liftIO $ (: []) <$> runRoomOpOn (appPodId app) (appGameRooms app) gameId roomOp
    RedisBroker conn _ -> do
      opId <- UUID.toText <$> liftIO UUID.nextRandom
      let key = roomOpsReplyKey opId
      -- This pod is subscribed to the channel like any other, so it answers
      -- through the same path rather than being special-cased here.
      tryRedis_
        $ runRedis conn
        $ publish roomOpsChannel
        $ BSL.toStrict
        $ Aeson.encode
        $ RoomOpRequest opId (gameIdToText gameId) roomOp
      liftIO $ threadDelay (roomOpCollectMicros roomOp)
      replies <- liftIO $ try @SomeException $ runRedis conn $ Redis.lrange key 0 (-1)
      tryRedis_ $ runRedis conn $ Redis.del (key :| [])
      pure case replies of
        Right (Right items) -> mapMaybe Aeson.decodeStrict items
        _ -> []

{- | Background heartbeat: every 'roomHeartbeatSeconds' refresh the seen
timestamp for every game this pod still has live subscribers for. This
keeps active games out of the staleness sweep even when nothing else
(subscribe / unsubscribe) is writing to Redis. Run once per pod via
'forkIO' from 'makeFoundation'.
-}
roomHeartbeat :: App -> IO ()
roomHeartbeat app = forever do
  threadDelay (roomHeartbeatSeconds * 1000000)
  rooms <- MVar.readMVar (appGameRooms app)
  active <- catMaybes <$> traverse keepIfActive (Map.toList rooms)
  case appMessageBroker app of
    WebSocketBroker -> pure ()
    RedisBroker conn _ -> unless (null active) do
      now <- currentEpoch
      void $ try @SomeException $ runRedis conn do
        for_ active \gid ->
          void $ hset roomsSeenHashKey ((roomField gid, BS8.pack (show now)) :| [])
  -- The same pass, because it asks the same question of the same rooms: this
  -- one about who is left on them rather than whether anyone is.
  sweepAbandonedRooms app
 where
  keepIfActive (gid, room) = do
    n <- roomClientCount room
    pure $ if n > 0 then Just gid else Nothing

{- | Close every room this pod holds that has been left to its spectators.

A game is over for a watcher the moment the last seat leaves it: nothing more
will happen on the board, and the socket would otherwise sit there forever
holding a room open on the server. So the room is closed out from under them,
with the notice that says why -- the same eviction an admin's delete performs,
and the client treats it the same way, by leaving rather than reconnecting.

Deliberately not done the instant the last player disconnects. A reload is a
disconnect, and a table of one that refreshes its page would otherwise take its
watchers down with it; 'roomAbandonedSeconds' is the grace that distinguishes
the two. Every pod sweeps its own rooms, and the player count is the cross-pod
one, so a pod whose own players have gone does not evict the watchers of a game
somebody is still playing on another.
-}
sweepAbandonedRooms :: App -> IO ()
sweepAbandonedRooms app = do
  rooms <- MVar.readMVar (appGameRooms app)
  now <- currentEpoch
  abandoned <- catMaybes <$> traverse (uncurry $ abandonedFor now) (Map.toList rooms)
  for_ abandoned (closeAbandonedRoom app)
 where
  abandonedFor now gid room = do
    subscribers <- roomClientCount room
    watchers <- roomWatcherCount room
    players <- seatedPlayerCount app gid (subscribers - watchers)
    -- An admin on the room is somebody deliberately looking at it. Closing it
    -- would throw them off mid-look, which is the one thing this sweep should
    -- never do; when they leave it is as abandoned as it ever was.
    admin <- roomHasAdmin room
    if admin || players > 0 || watchers <= 0 || subscribers <= 0
      then do
        atomically $ writeTVar (roomNoPlayersSince room) Nothing
        pure Nothing
      else do
        since <- atomically do
          current <- readTVar (roomNoPlayersSince room)
          case current of
            Just t -> pure t
            Nothing -> now <$ writeTVar (roomNoPlayersSince room) (Just now)
        pure $ if now - since >= roomAbandonedSeconds then Just gid else Nothing

{- | Seated players on a game across every pod, falling back to @onThisPod@
(this pod's own subscribers less its own watchers) when Redis cannot answer.
-}
seatedPlayerCount :: App -> ArkhamGameId -> Int -> IO Int
seatedPlayerCount app gameId onThisPod = case appMessageBroker app of
  WebSocketBroker -> pure onThisPod
  RedisBroker conn _ -> do
    result <- try @SomeException $ runRedis conn do
      total <- hget roomsHashKey (roomField gameId)
      watching <- hget watchersHashKey (roomField gameId)
      pure (total, watching)
    -- No entry for the game at all is Redis having lost sight of sockets this
    -- pod can see, not a game with nobody in it, so it falls back rather than
    -- reading as abandoned.
    pure case result of
      Right (Right (Just total), Right mWatching) -> readInt total - maybe 0 readInt mWatching
      _ -> onThisPod
 where
  readInt :: ByteString -> Int
  readInt = fromMaybe 0 . readMaybe . BS8.unpack

{- | Evict a room's remaining sockets and forget it, as
'Api.Handler.Arkham.Games.Shared.forceDeleteRoom' does for a deleted game --
same order, and for the same reason: the notice goes out while the subscribers
are still reading, and each drains its queue before stopping.
-}
closeAbandonedRoom :: App -> ArkhamGameId -> IO ()
closeAbandonedRoom app gameId = MVar.modifyMVar_ (appGameRooms app) \rooms -> do
  for_ (Map.lookup gameId rooms) \room -> do
    broadcastToRoom room $ Aeson.encode $ RoomClosed "abandoned"
    closeRoomSubscribers room
    tryRedis_ . join . readTVarIO $ roomUnsubscribe room
  pure $ Map.delete gameId rooms

{- | Every websocket room this pod is serving, across all three kinds.

A list of actions rather than the 'App' itself because 'applyMaintenance' is
called from the pub/sub handler, which is built in 'makeFoundation' BEFORE the
foundation exists -- the room maps do, so the handler closes over those.
-}
roomSources :: App -> [IO [Room]]
roomSources app =
  roomSourcesFrom (appGameRooms app) (appEventRooms app) (appThirdEditionRooms app)

-- | 'roomSources' for 'makeFoundation', which holds the maps but no 'App' yet.
roomSourcesFrom
  :: MVar (Map ArkhamGameId Room)
  -> MVar (Map ArkhamEpicEventId Room)
  -> MVar (Map UUID.UUID Room)
  -> [IO [Room]]
roomSourcesFrom games events tables =
  [ Map.elems <$> MVar.readMVar games
  , Map.elems <$> MVar.readMVar events
  , Map.elems <$> MVar.readMVar tables
  ]

{- | Put this pod into (or out of) maintenance mode and evict the sockets.

Order matters, and it is the same order 'Api.Handler.Arkham.Games.Shared.forceDeleteRoom'
uses: the notice is broadcast while the subscribers are still reading, then they
are closed, and each drains its queue before stopping -- so the client learns
WHY its socket went away instead of just seeing it drop and reconnecting into a
503. The rooms themselves are left in the map; they hold no resources without
subscribers, 'roomHeartbeat' skips empty ones, and keeping them means a
reconnect after the window lands on the room it left rather than rebuilding it.

Idempotent, which matters because the pod that publishes a change also receives
its own pub/sub message and applies it a second time.
-}
applyMaintenance :: TVar (Maybe Maintenance) -> [IO [Room]] -> Maybe Maintenance -> IO ()
applyMaintenance flag sources mMode = do
  atomically $ writeTVar flag mMode
  when (isJust mMode) do
    rooms <- concat <$> sequence sources
    for_ rooms \room -> do
      broadcastToRoom room $ Aeson.encode $ RoomClosed "maintenance"
      closeRoomSubscribers room

{- | Read the stored mode at pod startup, so a replica that starts (or is
restarted) during a window comes up refusing traffic rather than serving it
into a locked table.

Fails OPEN: a Redis blip here returns 'Nothing' and the pod serves normally.
The other way round, a brief Redis outage would take the whole site down and
leave no way to bring it back, since the switch itself needs Redis to clear.
-}
loadMaintenance :: Connection -> IO (Maybe Maintenance)
loadMaintenance conn = do
  res <- try @SomeException $ runRedis conn $ Redis.get maintenanceKey
  pure $ case res of
    Right (Right (Just raw)) -> Aeson.decodeStrict raw
    _ -> Nothing

{- | Turn maintenance mode on ('Just') or off ('Nothing') everywhere.

Applied locally first and published second. Without Redis -- development, and
any pod whose publish fails -- the local write is the entire mechanism, so the
switch still works on a single-replica deployment.
-}
setMaintenance :: (MonadIO m, HasApp m) => Maybe Maintenance -> m ()
setMaintenance mMode = do
  app <- getApp
  liftIO $ applyMaintenance (appMaintenance app) (roomSources app) mMode
  withRedis \conn -> runRedis conn do
    case mMode of
      Nothing -> void $ Redis.del (maintenanceKey :| [])
      Just mode -> void $ Redis.set maintenanceKey (BSL.toStrict $ Aeson.encode mode)
    void $ publish maintenanceChannel (BSL.toStrict $ Aeson.encode mMode)

{- | Pub/sub handler for 'maintenanceChannel'. A payload that does not decode
is ignored rather than guessed at -- taking the site down, or bringing it back
up mid-window, on the strength of a malformed message would be worse than
missing the message.
-}
onMaintenanceMessage :: TVar (Maybe Maintenance) -> [IO [Room]] -> ByteString -> IO ()
onMaintenanceMessage flag sources raw =
  for_ (Aeson.decodeStrict raw :: Maybe (Maybe Maintenance)) (applyMaintenance flag sources)

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
