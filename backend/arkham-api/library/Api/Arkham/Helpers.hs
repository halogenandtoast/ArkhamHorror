{-# LANGUAGE OverloadedRecordDot #-}

module Api.Arkham.Helpers where

import Arkham.Card
import Arkham.Classes hiding (Entity (..), select)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Debug
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.Metrics (withMetric)
import Arkham.Queue
import Arkham.Random
import Arkham.Tracing
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (try)
import Control.Lens hiding (from)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Random (MonadRandom (..), StdGen)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Data.UUID qualified as UUID
import Data.ByteString.Char8 qualified as BS8
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Esqueleto.Experimental
import Database.Redis (Connection, RedisChannel, hdel, hgetall, hincrby, hset, runRedis)
import Entity.Arkham.Game
import Entity.Arkham.LogEntry
import GHC.Records
import Import hiding (appLogger, appTracer, (==.), (>=.))
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (MonadTracer (..), inSpan')

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
  | GameCard {title :: Text, card :: Aeson.Value}
  | GameCardOnly {player :: PlayerId, title :: Text, card :: Aeson.Value}
  | GameTarot Aeson.Value
  | GameShowDiscard InvestigatorId
  | GameShowUnder InvestigatorId
  | GamePlayabilityInfo {cardId :: CardId, cardCode :: Text, checks :: [(Text, Maybe Text)]}
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
  , appTracer :: Trace.Tracer
  }

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

instance MonadTracer GameAppT where
  getTracer = asks appTracer

instance HasGameLogger GameAppT where
  getLogger = do
    logger <- asks appLogger
    pure $ \msg -> liftIO $ logger msg

instance Tracing GameAppT where
  type SpanType GameAppT = Trace.Span
  type SpanArgs GameAppT = Trace.SpanArguments
  -- See note in Arkham.GameT: addAttribute is a no-op so unused thunks
  -- (often `tshow` over deep ADTs) stay unforced.
  addAttribute _ _ _ = pure ()
  defaultSpanArgs = Trace.defaultSpanArguments
  doTrace name args action = withMetric name (inSpan' name args action)

gameIdToText :: ArkhamGameId -> Text
gameIdToText = UUID.toText . coerce

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Text -> m ()
noLogger = const (pure ())

gameChannel :: ArkhamGameId -> RedisChannel
gameChannel gameId = "arkham-" <> encodeUtf8 (tshow gameId)

getRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m Room
getRoom gid = do
  roomsVar <- getsApp appGameRooms
  liftIO do
    modifyMVar roomsVar \rooms -> do
      case Map.lookup gid rooms of
        Just r -> pure (rooms, r)
        Nothing -> do
          r <- newRoom (gameChannel gid)
          pure (Map.insert gid r rooms, r)

-- | Like 'getRoom' but never creates a Room. Use from the publish path so
-- that broadcasting to a game with no listeners doesn't leak an empty
-- Room into 'appGameRooms' that nothing will ever clean up.
lookupRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m (Maybe Room)
lookupRoom gid = do
  roomsVar <- getsApp appGameRooms
  Map.lookup gid <$> liftIO (MVar.readMVar roomsVar)

-- | Cross-server room registry. Two parallel Redis hashes:
--
--   * 'roomsHashKey'      gameId -> total WebSocket subscribers
--   * 'roomsSeenHashKey'  gameId -> unix epoch of last activity
--
-- Every subscribe/unsubscribe and a per-pod heartbeat refresh the seen
-- timestamp. On admin read, entries whose seen timestamp is older than
-- 'roomStaleSeconds' (or missing entirely) are treated as crashed-pod
-- cruft, removed from both hashes, and excluded from the response.
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

-- | Increment the cross-server client count for a game in Redis and
-- refresh its seen timestamp. No-op without a Redis broker.
incrRoomMember :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
incrRoomMember gameId = withRedis \conn -> runRedis conn do
  void $ hincrby roomsHashKey (roomField gameId) 1
  now <- liftIO currentEpoch
  void $ hset roomsSeenHashKey (roomField gameId) (BS8.pack (show now))

-- | Decrement the cross-server client count. When the new count is at or
-- below zero, drop the field from both hashes so the admin view doesn't
-- carry empty rooms; otherwise refresh the seen timestamp.
decrRoomMember :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
decrRoomMember gameId = withRedis \conn -> runRedis conn do
  result <- hincrby roomsHashKey (roomField gameId) (-1)
  case result of
    Right n | n <= 0 -> do
      void $ hdel roomsHashKey [roomField gameId]
      void $ hdel roomsSeenHashKey [roomField gameId]
    _ -> do
      now <- liftIO currentEpoch
      void $ hset roomsSeenHashKey (roomField gameId) (BS8.pack (show now))

-- | Aggregate client counts across servers from Redis, filtering out
-- entries whose 'seen' timestamp is older than 'roomStaleSeconds'. Stale
-- entries are also HDEL'd from both hashes so cruft from crashed pods
-- doesn't accumulate. Returns 'Nothing' when no Redis broker is
-- configured (callers fall back to local state).
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
sweepStaleRooms conn gameIds = void $ runRedis conn do
  let fields = map roomField gameIds
  void $ hdel roomsHashKey fields
  void $ hdel roomsSeenHashKey fields

-- | Background heartbeat: every 'roomHeartbeatSeconds' refresh the seen
-- timestamp for every game this pod still has live subscribers for. This
-- keeps active games out of the staleness sweep even when nothing else
-- (subscribe / unsubscribe) is writing to Redis. Run once per pod via
-- 'forkIO' from 'makeFoundation'.
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
          void $ hset roomsSeenHashKey (roomField gid) (BS8.pack (show now))
 where
  keepIfActive (gid, room) = do
    n <- roomClientCount room
    pure $ if n > 0 then Just gid else Nothing

displayCardType :: CardType -> Text
displayCardType = \case
  ActType -> "act"
  AgendaType -> "act"
  AssetType -> "asset"
  EventType -> "event"
  ScenarioType -> "scenario"
  SkillType -> "skill"
  StoryType -> "story"
  PlayerTreacheryType -> "treachery"
  PlayerEnemyType -> "enemy"
  TreacheryType -> "treachery"
  EnemyType -> "enemy"
  LocationType -> "location"
  EncounterAssetType -> "asset"
  EncounterEventType -> "event"
  InvestigatorType -> "investigator"
  KeyType -> "key"
  EnemyLocationCardType -> "enemy-location"

lockGame :: ArkhamGameId -> DB ()
lockGame gameId = void $ select do
  game <- from $ table @ArkhamGame
  where_ $ game.id ==. val gameId
  locking forUpdate

-- | One round-trip in the hot path: lock the row AND fetch its data.
-- Replaces the previous lockGame + get404 pair, halving the DB calls
-- for every caller of atomicallyWithGame on the success path.
-- (notFound lives in MonadHandler, but DB is rank-2 over MonadIO; on the
-- rare missing-game path we delegate to get404 to throw the 404, which
-- costs one extra empty SELECT only when the game doesn't exist.)
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

