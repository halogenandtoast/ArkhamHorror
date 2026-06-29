{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games.Shared where

import Api.Arkham.Epic (
  applyEpicDeltasLocked,
  lookupGameEvent,
  mkEpicEnv,
  modifySharedStateLocked,
  modifySharedStateLockedWith,
 )
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Ai.Decision (
  choiceFeatures,
  decideAi,
  decideAiAssist,
  flattenChoices,
  gatherSituation,
  isAssistCommitWindow,
  scoreBreakdown,
  unwrapQuestion,
 )
import Arkham.Ai.Helpers (lookupAiPlayer)
import Arkham.Ai.State (aiEnabled, defaultAiPlayerState)
import Arkham.Campaign.Types (CampaignAttrs)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.ClassSymbol
import Arkham.Classes.Entity (attr, toId)
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Difficulty
import Arkham.Entities (entitiesActs)
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedEventState,
  SharedKey (ActAdvanceGen, ActContribution, ActSpend, AdvanceRequested, AwaitingOrganizer, SharedActProgress),
  actProgressStages,
  epicEnvDeltaRef,
  epicEnvGroup,
  epicEnvSharedRef,
  groupOrdinalKey,
  setSharedCounter,
  sharedCounter,
  sharedCounters,
  sharedTotalInvestigators,
  totalInvestigatorsKey,
  updateSharedCounter,
 )
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State
import Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlayerId)
import Arkham.Message
import Arkham.Name
import Arkham.Queue
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Conduit
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Data.UUID (nil)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Redis (RedisChannel, msgMessage, pubSub, publish, runRedis, subscribe)
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.), (>=.))
import Import qualified as P
import Json
import Network.WebSockets (ConnectionException)
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import System.IO qualified as SIO
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception hiding (Handler)
import UnliftIO.Timeout (timeout)
import Yesod.WebSockets

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream gameId = catchingConnectionException do
  room <- lift $ getRoom gameId
  broker <- lift $ getsYesod appMessageBroker
  let broadcast = broadcastToRoom room

  let cleanup subId = do
        unsubscribeFromRoom room subId
        lift $ decrRoomMember gameId
        -- If this was the last subscriber, drop the room from the map so
        -- it doesn't accumulate orphaned entries.
        isEmpty <- liftIO $ atomically do
          IntMap.null <$> readTVar (roomSubscribers room)
        when isEmpty do
          roomsVar <- lift $ getsYesod appGameRooms
          liftIO $ modifyMVar_ roomsVar $ pure . Map.delete gameId
          lift $ removeChannel (gameChannel gameId)

  let acquire = do
        s <- subscribeToRoom room
        lift $ incrRoomMember gameId
        pure s

  bracket acquire (\(subId, _) -> cleanup subId) \(_subId, sub) -> do
    let Subscriber {subQueue, subOverflow} = sub
    mtid <- case broker of
      RedisBroker redisConn _ -> do
        tid <- liftIO
          $ async
          $ runRedis redisConn
          $ pubSub (subscribe [gameChannel gameId])
          $ \msg -> do
            broadcast (BSL.fromStrict $ msgMessage msg)
            pure mempty
        pure $ Just tid
      WebSocketBroker -> pure Nothing

    let stopSub = maybe (pure ()) (liftIO . cancel) mtid

    let sender =
          forever
            ( do
                msg <- atomically do
                  overflowed <- readTVar subOverflow
                  if overflowed
                    then throwSTM SlowSubscriber
                    else readTBQueue subQueue
                sendTextData msg
            )
            `catch` (\(_ :: SlowSubscriber) -> pure ())

    finally
      ( race_
          sender
          (runConduit $ sourceWS .| mapM_C (handleData room broadcast))
      )
      stopSub
 where
  handleData room broadcast dataPacket = lift do
    case eitherDecodeStrict dataPacket of
      Left err -> $(logWarn) $ tshow err
      Right answer ->
        updateGame answer gameId (Just room) `catch` \(e :: SomeException) -> do
          liftIO $ broadcast $ encode $ GameError $ tshow e

data SlowSubscriber = SlowSubscriber
  deriving stock Show
  deriving anyclass Exception

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ tshow (e :: ConnectionException)

{- | Generic read-only room subscription loop: bridge the Redis channel into the
room, fan room messages out to this websocket, and run @onLastLeave@ when the
final subscriber disconnects. Inbound client frames are ignored (read-only).
Used by the Epic Multiplayer event stream; 'gameStream' has its own variant
with per-game member counting and a log cache.
-}
streamRoom
  :: RedisChannel -> Room -> WebSocketsT Handler () -> WebSocketsT Handler ()
streamRoom channel room onLastLeave = catchingConnectionException do
  broker <- lift $ getsYesod appMessageBroker
  let broadcast = broadcastToRoom room
  let cleanup subId = do
        unsubscribeFromRoom room subId
        isEmpty <- liftIO $ atomically $ IntMap.null <$> readTVar (roomSubscribers room)
        when isEmpty onLastLeave
  bracket (subscribeToRoom room) (\(subId, _) -> cleanup subId) \(_subId, sub) -> do
    let Subscriber {subQueue, subOverflow} = sub
    mtid <- case broker of
      RedisBroker redisConn _ -> do
        tid <-
          liftIO
            $ async
            $ runRedis redisConn
            $ pubSub (subscribe [channel])
            $ \msg -> do
              broadcast (BSL.fromStrict $ msgMessage msg)
              pure mempty
        pure $ Just tid
      WebSocketBroker -> pure Nothing
    let stopSub = maybe (pure ()) (liftIO . cancel) mtid
    let sender =
          forever
            ( do
                msg <- atomically do
                  overflowed <- readTVar subOverflow
                  if overflowed then throwSTM SlowSubscriber else readTBQueue subQueue
                sendTextData msg
            )
            `catch` (\(_ :: SlowSubscriber) -> pure ())
    finally
      (race_ sender (runConduit $ sourceWS .| mapM_C (\(_ :: ByteString) -> pure ())))
      stopSub

data GetGameJson = GetGameJson
  { playerId :: Maybe PlayerId
  , multiplayerMode :: MultiplayerVariant
  , game :: PublicGame ArkhamGameId
  , eventId :: Maybe ArkhamEpicEventId
  {- ^ the Epic Multiplayer event this game is a group of, if any. Lets the client
  engage the event (shared state, start barrier, time limit) regardless of how
  the player reached the game (so it doesn't depend on a @?event@ URL query).
  -}
  }
  deriving stock (Show, Generic)

instance ToJSON GetGameJson where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data InvestigatorDetails = InvestigatorDetails
  { id :: InvestigatorId
  , classSymbol :: ClassSymbol
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data ScenarioDetails = ScenarioDetails
  { id :: ScenarioId
  , difficulty :: Difficulty
  , name :: Name
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data CampaignDetails = CampaignDetails
  { id :: CampaignId
  , difficulty :: Difficulty
  , currentCampaignMode :: Maybe TheDreamEaters.CampaignPart
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetails = GameDetails
  { id :: ArkhamGameId
  , scenario :: Maybe ScenarioDetails
  , campaign :: Maybe CampaignDetails
  , gameState :: GameState
  , name :: Text
  , investigators :: [InvestigatorDetails]
  , otherInvestigators :: [InvestigatorDetails]
  , multiplayerVariant :: MultiplayerVariant
  , hasOpenSeats :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetailsEntry = FailedGameDetails Text | SuccessGameDetails GameDetails
  deriving stock (Show, Generic)

instance ToJSON GameDetailsEntry where
  toJSON = \case
    FailedGameDetails t -> object ["error" .= t]
    SuccessGameDetails gd -> toJSON gd

{- | A broadcast callback. Used to fan out log lines and game-state updates
to every WebSocket subscriber on a room. May be a no-op if there are no
subscribers (e.g. a direct REST PUT with no client listening), in which
case messages are silently dropped instead of buffered indefinitely.
-}
type Broadcast = BSL.ByteString -> IO ()

{- | Hard cap on a single runMessages invocation. If a game's message
processing exceeds this we kill the action and roll back the surrounding
DB transaction so the worker (and the FOR UPDATE lock on the game row)
can be released. Empirically a normal action completes in well under 1s;
30s gives plenty of headroom for slow-but-legitimate scenario setup
while still preventing one poison game from monopolising a worker.
-}
runMessagesTimeoutMicros :: Int
runMessagesTimeoutMicros = 30 * 1000000

{- | Thrown by updateGame when 'runMessages' exceeds 'runMessagesTimeoutMicros'.
The Yesod handler turns this into a 500; the important effect is that the
exception propagates out of runDB, rolls back the transaction, and frees
the worker. Search Honeycomb / logs for this to find poison games.
-}
data RunMessagesTimeout = RunMessagesTimeout ArkhamGameId Int
  deriving stock Show
  deriving anyclass Exception

updateGame :: Answer -> ArkhamGameId -> Maybe Room -> Handler ()
updateGame response gameId mRoom = do
  let broadcast :: Broadcast
      broadcast = case mRoom of
        Nothing -> \_ -> pure ()
        Just room -> broadcastToRoom room
  tracer <- getTracer
  -- Imitation-learning capture flag, read once from app settings (a pure record
  -- read, no DB). When False the capture below is byte-for-byte inert.
  collectMl <- getsYesod (appCollectMlData . appSettings)
  -- NOTE: not wrapping the whole handler in withSpan_ -- it would rewrap
  -- Yesod's HCContent control-flow exceptions (notFound, notAuthenticated,
  -- sendStatusJSON, etc.) and break 404/401 responses (see Undo.hs note).
  -- The runMessages span below is wrapped where it's safe.
  (ArkhamGame {..}, oldLogEntries, updatedLog, mSharedUpdate, actAdvanced) <- runDB $ atomicallyWithGame gameId \g@ArkhamGame {..} -> do
    -- Read the prior log from the per-room cache when it's in sync with
    -- the just-locked game's step; otherwise fall back to the DB. Avoids
    -- the 217-row-avg getGameLog read on every action in the common case.
    oldLogEntries <-
      liftIO (lookupCachedLog mRoom arkhamGameStep) >>= \case
        Just entries -> pure entries
        Nothing -> gameLogToLogEntries <$> getGameLog gameId Nothing

    mLastStep <- getBy $ UniqueStep gameId arkhamGameStep
    let
      gameJson@Game {..} = arkhamGameCurrentData
      currentQueue =
        maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

    activePlayer <- runReaderT getActivePlayer gameJson

    let playerId = fromMaybe activePlayer (answerPlayer response)

    -- AI seats answer their own parked question: resolve an AiAnswer into a
    -- concrete Answer by running the read-only decision engine over the parked
    -- game (HasGame (ReaderT Game m), the same pattern as getActivePlayer
    -- above), then feed it through the normal handleAnswer path so all
    -- downstream message synthesis is reused. A disabled/unregistered seat or an
    -- absent question resolves to Nothing and is treated as a no-op. decideAi
    -- sets qrQuestionVersion from this same parked game, so it matches
    -- gameScenarioSteps at apply time (no "Stale question").
    mResolved <- case response of
      AiAnswer aiPid -> case lookupAiPlayer aiPid gameSettings of
        Just st | aiEnabled st -> case Map.lookup aiPid gameQuestion of
          Just question -> do
            -- The non-performer skill-test commit window re-asks itself after
            -- every commit/uncommit and offers no Start/Done, so the auto-drive
            -- decideAi would loop forever. Decline it (leave the seat parked,
            -- exactly like a disabled seat / absent question); the performer
            -- starting the test silently drops the parked window. On-demand
            -- single commits arrive separately as AiAssist below.
            isAssist <- runReaderT (isAssistCommitWindow aiPid question) gameJson
            if isAssist
              then pure Nothing
              else Just <$> runReaderT (decideAi st aiPid question) gameJson
          Nothing -> pure Nothing
        _ -> pure Nothing
      -- AiAssist: commit one card from this seat's parked assist window via the
      -- assist decision engine. Just ans -> apply through the normal answer path
      -- (commits exactly one card); Nothing -> no-op (nothing worth adding).
      AiAssist aiPid -> case lookupAiPlayer aiPid gameSettings of
        Just st | aiEnabled st -> case Map.lookup aiPid gameQuestion of
          Just question -> runReaderT (decideAiAssist aiPid question) gameJson
          Nothing -> pure Nothing
        _ -> pure Nothing
      _ -> pure (Just response)

    logRef <- newIORef []
    reply <- case mResolved of
      Nothing -> pure (Unhandled "AI seat disabled or no parked question")
      Just resolved -> handleAnswer gameJson playerId resolved
    case reply of
      Unhandled _ -> pure (g, oldLogEntries, [], Nothing, False)
      Handled answerMessages -> do
        -- Epic Multiplayer: if this game is a group within an event, build an
        -- EpicEnv so Shared* messages emitted during the action are captured as
        -- deltas. 'Nothing' (every ordinary game) means zero behavior change.
        mEpicCtx <- lookupGameEvent gameId
        mEpicEnv <- traverse (uncurry mkEpicEnv) mEpicCtx

        -- Epic Multiplayer: mirror the current shared counters into this group's
        -- scenario state (as EpicShared counts) before the action runs, so the
        -- scenario/enemy read up-to-date shared values purely. Refreshed every
        -- action (pull), keyed by sharedKeyText, plus the frozen total.
        syncMsgs <- case mEpicEnv of
          Nothing -> pure []
          Just epic -> epicSyncMessages (epicEnvGroup epic) <$> liftIO (readIORef (epicEnvSharedRef epic))

        let
          messages =
            [SetActivePlayer playerId | activePlayer /= playerId]
              <> answerMessages
              <> [SetActivePlayer activePlayer | activePlayer /= playerId]
        gameRef <- newIORef gameJson
        queueRef <- newQueue ((ClearUI : syncMsgs <> messages) <> currentQueue)
        genRef <- newIORef $ mkStdGen gameSeed

        -- Circuit breaker: cap runMessages at runMessagesTimeoutMicros so a
        -- pathological game state (infinite loop / message-handler explosion)
        -- can't hold a worker hostage and pin a FOR UPDATE lock on the game
        -- row indefinitely. On timeout, throw RunMessagesTimeout -- this
        -- aborts the surrounding DB transaction (rollback releases the lock)
        -- and lets the worker return to the pool.
        mResult <- liftIO $ timeout runMessagesTimeoutMicros do
          runGameApp (GameApp gameRef queueRef genRef (handleMessageLog logRef broadcast) tracer mEpicEnv) do
            runMessages (gameIdToText gameId) Nothing
        case mResult of
          Just () -> pure ()
          Nothing -> liftIO $ throwIO $ RunMessagesTimeout gameId runMessagesTimeoutMicros

        ge <- readIORef gameRef
        let diffDown = diff ge arkhamGameCurrentData
        -- Epic Multiplayer: detect an IN-GROUP act advance (the act entity is
        -- replaced on advance/loop) so we can wall off undo across it. Epic games
        -- only; cheap (acts in play is ~1).
        let actAdvanced =
              isJust mEpicCtx
                && epicActFingerprint arkhamGameCurrentData
                /= epicActFingerprint ge

        updatedQueue <- readIORef $ queueToRef queueRef
        -- handleMessageLog conses for O(1) inserts; reverse here to restore order.
        updatedLog <- reverse <$> readIORef logRef

        now <- liftIO getCurrentTime
        deleteWhere [ArkhamStepArkhamGameId P.==. gameId, ArkhamStepStep P.>. arkhamGameStep]
        let g' =
              ArkhamGame
                arkhamGameName
                ge
                (arkhamGameStep + 1)
                arkhamGameMultiplayerVariant
                arkhamGameCreatedAt
                now
        replace gameId g'
        insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
        void
          $ upsertBy
            (UniqueStep gameId (arkhamGameStep + 1))
            ( ArkhamStep
                gameId
                (Choice diffDown updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL ge)
            )
            [ ArkhamStepChoice =. Choice diffDown updatedQueue
            , ArkhamStepActionDiff =. ActionDiff (view actionDiffL ge)
            ]

        -- Imitation-learning capture (gated off by default; human-only;
        -- multi-choice only). Runs against gameJson = the PARKED state S_{k-1}
        -- (its gameQuestion is the question the human just answered), keyed by
        -- the produced step (arkhamGameStep + 1) so group_ids match the
        -- historical arkham-extract dataset. Internally try/savepoint-guarded:
        -- a capture failure can never abort or alter this action.
        captureMlDecision collectMl gameId (arkhamGameStep + 1) gameJson playerId response now

        -- Epic Multiplayer: drain any shared-counter deltas emitted this action
        -- and apply them to the authoritative event row under a short FOR UPDATE
        -- lock (taken late, only when there are deltas), within this same
        -- transaction so the game step and shared mutation commit atomically.
        mSharedUpdate <- case mEpicCtx of
          Just (eventEntity, _) -> do
            deltas <- maybe (pure []) (liftIO . readIORef . epicEnvDeltaRef) mEpicEnv
            if null deltas
              then pure Nothing
              else do
                s <-
                  applyEpicDeltasLocked
                    (entityKey eventEntity)
                    (Just gameId)
                    (Just (arkhamGameStep + 1))
                    deltas
                pure $ Just (entityKey eventEntity, s)
          Nothing -> pure Nothing

        pure (g', oldLogEntries, updatedLog, mSharedUpdate, actAdvanced)

  -- Update the per-room cache after the DB transaction has committed,
  -- so the cache is never ahead of durably-stored state.
  let publishLog = oldLogEntries <> updatedLog
  liftIO $ writeCachedLog mRoom arkhamGameStep publishLog

  publishToRoom gameId
    $ GameUpdate
    $ PublicGame
      gameId
      arkhamGameName
      publishLog
      arkhamGameCurrentData

  -- Epic Multiplayer: propagate a shared-counter change across the event — update
  -- every client's shared store AND sync the other groups' game-state boards to
  -- it (so countermeasures / blob health change live in every group, not just on
  -- that group's next action). The acting group is skipped (already reflected).
  for_ mSharedUpdate \(eid, s) -> propagateShared eid (Just gameId) s

  -- Epic Multiplayer: wall off undo across an IN-GROUP act advance. Each group
  -- advances its own act via the normal AdvanceAct flow (no cross-group injection);
  -- when this action advanced the act, set the per-game undo floor to the committed
  -- step so it can't be locally undone (the other groups follow on their own turns
  -- via 'ActAdvanceGen'). 'arkhamGameStep' here is the post-commit (new) step.
  when actAdvanced $ setGameUndoFloor gameId arkhamGameStep

  -- Epic Multiplayer: PURE-COUNTER coordinator. When a group's act raised
  -- 'AdvanceRequested', atomically (under the event lock) bump the shared
  -- 'ActAdvanceGen' exactly once per pool crossing, using the pool reset as the
  -- once-only token. It touches ONLY shared counters — it never injects messages
  -- into any group's game (that injection was clobbering followers' encounter draws).
  for_ mSharedUpdate \(eid, s) -> coordinateEpicActAdvance eid s

{- | Read the cached log entries IF the cache is consistent with the locked
game's current step. Returns Nothing on a mismatch (so the caller refetches
from the DB and refreshes the cache).
-}
lookupCachedLog :: Maybe Room -> Int -> IO (Maybe [Text])
lookupCachedLog Nothing _ = pure Nothing
lookupCachedLog (Just room) currentStep = atomically do
  cachedVal <- readTVar (roomLogCache room)
  pure $ case cachedVal of
    Just c | c.cacheStep == currentStep -> Just c.cacheEntries
    _ -> Nothing

{- | Write the cache after a successful update. The step recorded is the new
post-update step; the next action will read the game at that step and find
a consistent cache.
-}
writeCachedLog :: Maybe Room -> Int -> [Text] -> IO ()
writeCachedLog Nothing _ _ = pure ()
writeCachedLog (Just room) newStep entries =
  atomically $ writeTVar (roomLogCache room) $ Just $ RoomLogCache newStep entries

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

handleMessageLog
  :: MonadIO m => IORef [Text] -> Broadcast -> ClientMessage -> m ()
handleMessageLog logRef broadcast msg = liftIO $ do
  -- Cons in O(1); the caller reverses once when reading the IORef.
  -- The previous (logs <> [txt]) was O(n) per call -> O(n^2) per action,
  -- which mattered during scenario setup with hundreds of log lines.
  for_ (toClientText msg) $ \txt ->
    atomicModifyIORef' logRef (\logs -> (txt : logs, ()))
  broadcast (encode $ toGameMessage msg)
 where
  toGameMessage = \case
    ClientText txt -> GameMessage txt
    ClientError txt -> GameError txt
    ClientUI txt -> GameUI txt
    ClientAudio txt -> GameAudio txt
    ClientCard t v -> GameCard t v
    ClientCardOnly i t v -> GameCardOnly i t v
    ClientTarot v -> GameTarot v
    ClientShowDiscard v -> GameShowDiscard v
    ClientShowUnder v -> GameShowUnder v
    ClientPlayabilityReport cid cc chks -> GamePlayabilityInfo cid cc chks
  toClientText = \case
    ClientText txt -> Just txt
    ClientError {} -> Nothing
    ClientUI {} -> Nothing
    ClientAudio {} -> Nothing
    ClientCard {} -> Nothing
    ClientCardOnly {} -> Nothing
    ClientTarot {} -> Nothing
    ClientShowDiscard {} -> Nothing
    ClientShowUnder {} -> Nothing
    ClientPlayabilityReport {} -> Nothing

publishToRoom :: (MonadIO m, ToJSON a, HasApp m) => ArkhamGameId -> a -> m ()
publishToRoom gameId a = do
  broker <- getsApp appMessageBroker
  case broker of
    RedisBroker redisConn _ ->
      void
        $ liftIO
        $ runRedis redisConn
        $ publish (gameChannel gameId)
        $ toStrictByteString
        $ encode a
    WebSocketBroker ->
      -- Don't create a Room here. If nobody is subscribed, drop the
      -- update on the floor; the next subscriber will read the latest
      -- state from the database when they connect.
      lookupRoom gameId >>= traverse_ (`broadcastToRoom` encode a)

-- | Epic Multiplayer sibling of 'publishToRoom', keyed by event id.
publishToEventRoom :: (MonadIO m, ToJSON a, HasApp m) => ArkhamEpicEventId -> a -> m ()
publishToEventRoom eid a = do
  broker <- getsApp appMessageBroker
  case broker of
    RedisBroker redisConn _ ->
      void
        $ liftIO
        $ runRedis redisConn
        $ publish (eventChannel eid)
        $ toStrictByteString
        $ encode a
    WebSocketBroker ->
      lookupEventRoom eid >>= traverse_ (`broadcastToRoom` encode a)

-- | The (non-null) game ids of every group in an event.
getEventGroupGameIds :: ArkhamEpicEventId -> Handler [ArkhamGameId]
getEventGroupGameIds eid = do
  rows <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    pure grp.arkhamGameId
  pure $ mapMaybe (\(Value m) -> m) rows

{- | Each group's @(ordinal, game id)@ (ordinal order), for groups that have a
game. Used to mirror the per-group ordinal into scenario state during sync and
by 'propagateShared' to fan a shared-state change out to every group.
-}
getEventGroupGroups :: ArkhamEpicEventId -> Handler [(Int, ArkhamGameId)]
getEventGroupGroups eid = do
  rows <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    orderBy [asc grp.ordinal]
    pure (grp.ordinal, grp.arkhamGameId)
  pure [(ordinal, gid) | (Value ordinal, Value (Just gid)) <- rows]

{- | Broadcast a shared-state update to the event's dashboard feed AND to every
group's own game stream, so all connected clients (organizer dashboard,
organizer bars, shared displays) reflect the new shared counters live.
-}
broadcastSharedToEvent :: ArkhamEpicEventId -> SharedEventState -> Handler ()
broadcastSharedToEvent eid s = do
  publishToEventRoom eid (SharedStateUpdate s)
  gameIds <- getEventGroupGameIds eid
  for_ gameIds \gid -> publishToRoom gid (SharedStateUpdate s)

{- | The ScenarioCountSet messages that mirror the authoritative shared counters
into a group's scenario state (as EpicShared counts), keyed by sharedKeyText,
plus the frozen total and this group's own ordinal. The scenario/enemy
reconcile their local board representations (Resource tokens, Subject 8L-08
health) from these; a card can read 'groupOrdinalKey' to learn which group it is
and 'ActAdvanceGen' to learn when it is behind on advancing its act.
-}
epicSyncMessages :: GroupOrdinal -> SharedEventState -> [Message]
epicSyncMessages (GroupOrdinal ordinal) shared =
  -- total-investigators FIRST: entities that derive a value from it (e.g. Subject
  -- 8L-08's max health = 15 * total) must see it before their own counter syncs.
  ScenarioCountSet (EpicShared totalInvestigatorsKey) (sharedTotalInvestigators shared)
    : ScenarioCountSet (EpicShared groupOrdinalKey) ordinal
    : [ScenarioCountSet (EpicShared k) v | (k, v) <- Map.toList (sharedCounters shared)]

{- | Server-initiated run of @msgs@ inside one group's game, under the same
FOR UPDATE lock / GameApp machinery a normal action uses. Runs only when the
game is still actively playing AND @p@ holds for its current state; the
predicate is evaluated INSIDE the lock so concurrent callers serialize on it
(e.g. the Epic time-up forcing checks the agenda stage here so duplicate
countdown-expiry calls can't double-advance). Persists a new step whose pending
queue is the queue produced by the run (e.g. the continuation of a question the
run parked) followed by the group's previously-pending queue, then broadcasts
the GameUpdate. Games that are not active, or fail @p@, are left untouched.
appEvent = Nothing: these server-initiated runs must not themselves emit shared
deltas (they reconcile board state directly), so there is no feedback loop.
-}
runMessagesInGroupWhen :: (Game -> Bool) -> [Message] -> ArkhamGameId -> Handler ()
runMessagesInGroupWhen p msgs gid = void $ runMessagesInGroupCore p msgs gid

{- | The core of 'runMessagesInGroupWhen'. Persists a new step with an EMPTY
down-patch: every server-initiated group run (board sync, time-up forcing, the
act-advance spend/flip) is reconciled forward and is NOT independently undoable
— the act-advance spend/flip is instead walled off by the per-game undo FLOOR
('Api.Arkham.Epic.getGameUndoFloor'), and board syncs are re-derived on the
next propagate. Returns the new 'ArkhamGame' (with its new step) so callers can
read the post-run step (e.g. to set that floor).
-}
runMessagesInGroupCore
  :: (Game -> Bool) -> [Message] -> ArkhamGameId -> Handler (Maybe ArkhamGame)
runMessagesInGroupCore p msgs gid = do
  tracer <- getTracer
  now <- liftIO getCurrentTime
  mUpdate <- runDB $ atomicallyWithGame gid \ArkhamGame {..} ->
    case gameGameState arkhamGameCurrentData of
      IsActive | p arkhamGameCurrentData -> do
        mLastStep <- getBy (UniqueStep gid arkhamGameStep)
        let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep
        gameRef <- liftIO $ newIORef arkhamGameCurrentData
        queueRef <- liftIO $ newQueue msgs
        genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))
        liftIO
          $ runGameApp (GameApp gameRef queueRef genRef (pure . const ()) tracer Nothing)
          $ runMessages (gameIdToText gid) Nothing
        updatedGame <- liftIO $ readIORef gameRef
        -- The queue left after the run: empty for a pure board sync (it drains to
        -- empty), or the continuation of a question the run parked (e.g. the
        -- lead-player confirm of a forced agenda advance). Persist it AHEAD of the
        -- group's previously-pending queue so a forced advance resolves to
        -- completion while an undisturbed sync simply preserves the in-flight
        -- queue (producedQueue is [] there).
        producedQueue <- liftIO $ readIORef (queueToRef queueRef)
        let
          game' =
            ArkhamGame
              arkhamGameName
              updatedGame
              (arkhamGameStep + 1)
              arkhamGameMultiplayerVariant
              arkhamGameCreatedAt
              now
        replace gid game'
        insert_
          $ ArkhamStep
            gid
            (Choice mempty (producedQueue <> currentQueue))
            (arkhamGameStep + 1)
            (ActionDiff $ view actionDiffL updatedGame)
        pure (Just game')
      _ -> pure Nothing
  for_ mUpdate \g' ->
    publishToRoom gid
      $ GameUpdate
      $ PublicGame gid (arkhamGameName g') [] (arkhamGameCurrentData g')
  pure mUpdate

-- | 'runMessagesInGroupWhen' with no extra guard beyond the game being active.
runMessagesInGroup :: [Message] -> ArkhamGameId -> Handler ()
runMessagesInGroup = runMessagesInGroupWhen (const True)

{- | Set (upsert) a group game's undo FLOOR to @step@: undo can no longer cross it
(enforced in 'Api.Handler.Arkham.Undo'). Called for every group an act advance
settled, with that group's post-advance persistence step. Floors only ever
increase (each settlement runs at a later step), so an unconditional set is
monotonic.
-}
setGameUndoFloor :: ArkhamGameId -> Int -> Handler ()
setGameUndoFloor gid step =
  runDB
    $ void
    $ P.upsertBy
      (UniqueGameUndoFloor gid)
      (ArkhamGameUndoFloor gid step)
      [ArkhamGameUndoFloorFloorStep P.=. step]

{- | Server-initiated sync of one (other) group's game state to the current
shared counters, so its BOARD (countermeasures, blob health) reflects the
change live without that group having to take an action. Runs only the sync
messages (the group's own pending queue/question is preserved), persists a new
step, and broadcasts the resulting GameUpdate. Skips games that aren't active.
-}
syncOneGroup :: GroupOrdinal -> SharedEventState -> ArkhamGameId -> Handler ()
syncOneGroup ordinal shared = runMessagesInGroup (epicSyncMessages ordinal shared)

{- | Propagate a shared-state change across an event: update every client's
shared store (organizer dashboard/bars) AND sync each group's game-state board
to it. @mOrigin@ (the acting group) is skipped — its own action already
reflected the change locally.
-}
propagateShared :: ArkhamEpicEventId -> Maybe ArkhamGameId -> SharedEventState -> Handler ()
propagateShared eid mOrigin shared = do
  broadcastSharedToEvent eid shared
  groups <- getEventGroupGroups eid
  for_ groups \(ordinal, gid) ->
    when (Just gid /= mOrigin)
      $ syncOneGroup (GroupOrdinal ordinal) shared gid
      `catch` \(e :: SomeException) ->
        $(logWarn) $ "Epic syncOneGroup failed for " <> tshow gid <> ": " <> tshow e

{- | Identity fingerprint of the act(s) in play, used to detect an IN-GROUP act
advance (the act entity is replaced on advance/loop) so 'updateGame' can set the
per-game undo floor. Acts in play is normally a singleton.
-}
epicActFingerprint :: Game -> [ActId]
epicActFingerprint game = sort [attr (.id) act | act <- toList (entitiesActs (gameEntities game))]

{- | Epic Multiplayer PURE-COUNTER cross-group coordinator, narrowed to a GATE
SETTER. Runs POST-COMMIT (in 'updateGame') whenever a group's just-committed action
raised 'AdvanceRequested'. For each such stage, under the event @FOR UPDATE@ lock:
if the pool ('SharedActProgress') has reached threshold (@2 * total@) it sets
@AwaitingOrganizer N = 1@ (idempotent — arming an already-armed gate is a no-op);
it ALWAYS clears 'AdvanceRequested'. Then broadcasts.

It does NOT reset the pool, bump 'ActAdvanceGen', or floor anything — CONSUMPTION
now happens at allocation time in the organizer endpoint
('Api.Handler.Arkham.Events.postApiV1ArkhamEventResolveAdvanceR' →
'settleOrganizerAdvance'). The gate just raises the organizer overlay. Touches ONLY
shared counters; never injects a gameplay message into any group.
-}
coordinateEpicActAdvance :: ArkhamEpicEventId -> SharedEventState -> Handler ()
coordinateEpicActAdvance eid s =
  for_ (actProgressStages s) \stage ->
    when (sharedCounter (AdvanceRequested stage) s > 0) do
      newState <- runDB $ modifySharedStateLocked eid \st ->
        let
          pool = sharedCounter (SharedActProgress stage) st
          threshold = 2 * sharedTotalInvestigators st
          gate =
            if threshold > 0 && pool >= threshold
              then setSharedCounter (AwaitingOrganizer stage) 1
              else Import.id
         in
          gate (setSharedCounter (AdvanceRequested stage) 0 st)
      broadcastSharedToEvent eid newState

{- | Floor undo for EVERY group in the event at its CURRENT persistence step,
making a consuming act advance a global checkpoint: no group can undo across it (so
no contributor can rewind a now-consumed pool placement), while every group's
actions AFTER it stay undoable. Floors are monotonic (always set at a later step
than any prior floor). Called ONLY from the consuming claim in
'coordinateEpicActAdvance'.
-}
floorAllGroupsAtCurrentStep :: ArkhamEpicEventId -> Handler ()
floorAllGroupsAtCurrentStep eid = do
  gameIds <- getEventGroupGameIds eid
  for_ gameIds \gid -> do
    mStep <- runDB $ selectOne do
      g <- from $ table @ArkhamGame
      where_ $ g.id ==. val gid
      pure g.step
    for_ mStep \(Value step) -> setGameUndoFloor gid step

-- | Each group's @(ordinal, contribution)@ toward a stage-@stage@ advance, read
-- from the authoritative shared 'ActContribution' counters (mirrored from the
-- contributing acts). Shaped for the organizer endpoint to cap each group's spend.
getEventGroupContributions :: ArkhamEpicEventId -> Int -> Handler [(Int, Int)]
getEventGroupContributions eid stage = do
  mEvent <- runDB $ selectOne do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    pure e
  case mEvent of
    Nothing -> pure []
    Just (Entity _ event) -> do
      let shared = arkhamEpicEventSharedState event
      groups <- getEventGroupGroups eid
      pure [(ordinal, sharedCounter (ActContribution stage (GroupOrdinal ordinal)) shared) | (ordinal, _gid) <- groups]

{- | Apply an organizer's act-advance allocation for a stage. Atomic + idempotent:
under the event @FOR UPDATE@ lock, ONLY if @AwaitingOrganizer stage == 1@ (so a
double-submit no-ops), it writes each group's 'ActSpend', resets the pool, bumps
'ActAdvanceGen', and clears 'AwaitingOrganizer'.

CRITICAL ORDERING when it applied: mirror the new shared state into EVERY group's
replica FIRST (so the parked group's advance handler can read its 'ActSpend' from
its replica), THEN floor every group at the consumption checkpoint, and only THEN
broadcast — the broadcast clears 'AwaitingOrganizer' on the event store, which lifts
the organizer overlay and lets the parked group proceed. No gameplay message is ever
injected into any group; the seam moves shared counters only.
-}
settleOrganizerAdvance :: ArkhamEpicEventId -> Int -> Map Int Int -> Handler ()
settleOrganizerAdvance eid stage spendByOrdinal = do
  (newState, applied) <- runDB $ modifySharedStateLockedWith eid \st ->
    if sharedCounter (AwaitingOrganizer stage) st /= 1
      then (st, False)
      else
        let
          withSpends =
            foldl'
              (\acc (ordinal, spend) -> setSharedCounter (ActSpend stage (GroupOrdinal ordinal)) spend acc)
              st
              (Map.toList spendByOrdinal)
          st' =
            setSharedCounter (AwaitingOrganizer stage) 0
              . updateSharedCounter (+ 1) (ActAdvanceGen stage)
              . setSharedCounter (SharedActProgress stage) 0
              $ withSpends
         in
          (st', True)
  when applied do
    -- (1) mirror into every group's replica BEFORE lifting the overlay
    groups <- getEventGroupGroups eid
    for_ groups \(ordinal, gid) ->
      syncOneGroup (GroupOrdinal ordinal) newState gid
        `catch` \(e :: SomeException) ->
          $(logWarn) $ "Epic settle mirror failed for " <> tshow gid <> ": " <> tshow e
    -- (2) global undo checkpoint: no group can rewind across the consumption
    floorAllGroupsAtCurrentStep eid
    -- (3) broadcast LAST: clears AwaitingOrganizer -> lifts the overlay
    broadcastSharedToEvent eid newState

toGameDetailsEntry :: Entity ArkhamGameRaw -> Int -> GameDetailsEntry
toGameDetailsEntry (Entity gameId game) playerCount =
  case fromJSON @Game (arkhamGameRawCurrentData game) of
    Success a ->
      let
        investigators =
          map (\(i :: Investigator) -> InvestigatorDetails i.id i.classSymbol)
            $ toList a.gameEntities.investigators
        variant = arkhamGameRawMultiplayerVariant game
       in
        SuccessGameDetails
          $ GameDetails
            { id = coerce gameId
            , scenario = case a.gameMode of
                This _ -> Nothing
                That s -> Just $ ScenarioDetails s.id s.difficulty s.name
                These _ s -> Just $ ScenarioDetails s.id s.difficulty s.name
            , campaign = case a.gameMode of
                This c -> Just $ CampaignDetails c.id c.difficulty c.currentCampaignMode
                That _ -> Nothing
                These c _ -> Just $ CampaignDetails c.id c.difficulty c.currentCampaignMode
            , gameState = a.gameGameState
            , name = arkhamGameRawName game
            , investigators
            , otherInvestigators =
                let
                  ins = case a.gameMode of
                    This c -> campaignOtherInvestigators (toJSON c.meta)
                    That _ -> mempty
                    These c _ -> campaignOtherInvestigators (toJSON c.meta)
                 in
                  map (\i -> InvestigatorDetails i.id i.classSymbol) ins
            , multiplayerVariant = variant
            , hasOpenSeats = variant == WithFriends && playerCount < length investigators
            }
    Error e -> FailedGameDetails ("Failed to load " <> tshow gameId <> ": " <> T.pack e)
 where
  campaignOtherInvestigators j = case parse (withObject "" (.: "otherCampaignAttrs")) j of
    Error _ -> mempty
    Success (attrs :: CampaignAttrs) -> map (`lookupInvestigator` PlayerId nil) $ Map.keys attrs.decks

deleteRoom :: ArkhamGameId -> Handler ()
deleteRoom gameId = do
  roomsVar <- getsYesod appGameRooms
  liftIO $ modifyMVar_ roomsVar $ pure . Map.delete gameId

deleteEventRoom :: ArkhamEpicEventId -> Handler ()
deleteEventRoom eid = do
  roomsVar <- getsYesod appEventRooms
  liftIO $ modifyMVar_ roomsVar $ pure . Map.delete eid

-- ---------------------------------------------------------------------------
-- Imitation-learning capture
--
-- Logs one 'ArkhamMlDecision' row per HUMAN, multi-choice decision, against the
-- CURRENT engine (no replay -> drift-free), with the chosen index as a direct
-- label. Reuses the exported AI feature functions ('gatherSituation',
-- 'choiceFeatures', 'scoreBreakdown') so the live dataset distils against the
-- same scorer 'decideAi' uses; it is NOT a second feature implementation.
--
-- Safety contract (this is on the hot path, inside the FOR UPDATE-locked
-- transaction): when @collectMl@ is False it is a no-op (no query, no insert);
-- otherwise it adds at most ONE insert per human decision, and every failure
-- mode is swallowed -- see 'runMlCapture' for why even a SQL-level failure
-- (e.g. the table was never created) cannot abort or alter the game action.

{- | Capture one decision when @collectMl@ is on AND the decision qualifies (see
'mlDecisionRows'). The PARKED game @game@ is S_{k-1}: its 'gameQuestion' is the
question the human just answered, and @response@ is that human's original answer
(an 'AiAnswer'/'AiAssist' seat is rejected by 'mlDecisionRows', so only human
labels are recorded). @step@ is the PRODUCED step (so the @group_id@ matches the
historical extractor). Runs in the same 'DB' transaction the action commits in.
-}
captureMlDecision
  :: MonadIO m
  => Bool
  -> ArkhamGameId
  -> Int
  -> Game
  -> PlayerId
  -> Answer
  -> UTCTime
  -> ReaderT SqlBackend m ()
captureMlDecision collectMl gameId step game pid response now =
  when collectMl $ do
    conn <- ask
    -- Hand the RAW inputs to the IO guard: the qualifying check + feature build
    -- (mlDecisionRows) AND the insert all run inside 'handleAny', so NOTHING --
    -- not even a blowup while deciding to skip -- can escape into this action's
    -- transaction.
    liftIO $ runMlCapture conn gameId step game pid response now

{- | Decide-and-insert the capture row so it can NEVER break the surrounding
action. Everything risky runs inside 'handleAny':

  1. 'mlDecisionRows' (the skip/keep decision + lazy feature build) is evaluated
     here, so a partial-function blowup deep in the AI feature code is caught and
     the transaction is never touched.
  2. The feature/breakdown JSON is forced before any SQL is issued.
  3. The insert is wrapped in a SQL @SAVEPOINT@: a SQL-level failure (most
     plausibly: @collect-ml@ was enabled before the @arkham_ml_decisions@ table
     was created) is rolled back to the savepoint, which un-poisons the
     surrounding transaction so the game step still commits. Without the
     savepoint a failed statement would abort the whole transaction.

In every case it logs and continues.
-}
runMlCapture
  :: SqlBackend -> ArkhamGameId -> Int -> Game -> PlayerId -> Answer -> UTCTime -> IO ()
runMlCapture conn gameId step game pid response now = handleAny logMlError
  $ case mlDecisionRows game pid response of
    Nothing -> pure ()
    Just (chosenIndex, rowsValue) -> do
      -- (2) force the lazy feature JSON before any SQL touches the connection.
      _ <- evaluate (BSL.length (encode rowsValue))
      let decision = ArkhamMlDecision gameId step (tshow pid) chosenIndex rowsValue now
      -- (3) savepoint-guarded insert on the action's own connection/transaction.
      flip runReaderT conn $ do
        rawExecute "SAVEPOINT arkham_ml_capture" []
        res <- try (insert_ decision)
        case res :: Either SomeException () of
          Right () -> rawExecute "RELEASE SAVEPOINT arkham_ml_capture" []
          Left e -> do
            rawExecute "ROLLBACK TO SAVEPOINT arkham_ml_capture" []
            rawExecute "RELEASE SAVEPOINT arkham_ml_capture" []
            liftIO $ logMlError e

logMlError :: SomeException -> IO ()
logMlError e =
  SIO.hPutStrLn SIO.stderr $ "[arkham-ml] capture skipped (continuing): " <> show e

{- | The qualifying gate + the per-choice feature rows, or 'Nothing' to skip.
Skips unless ALL hold (mirrors the arkham-extract filters):

  * the original answer is a plain human @Answer (QuestionResponse ..)@ -- this
    alone rejects AI seats ('AiAnswer'/'AiAssist') and the non-index answer
    shapes (amounts/payment/raw/deck);
  * the seat has a parked question whose flattened choices number > 1.

When it qualifies it returns @(chosenIndex, rows)@ where @rows@ is the lazy
jsonb array -- one element per flattened choice -- to be forced inside the guard.
The situation snapshot is read in @ReaderT Game Identity@, exactly how 'decideAi'
invokes 'gatherSituation'.
-}
mlDecisionRows :: Game -> PlayerId -> Answer -> Maybe (Int, Json.Value)
mlDecisionRows game pid = \case
  Answer (QuestionResponse {qrChoice}) -> do
    q <- Map.lookup pid (gameQuestion game)
    (cs, _off) <- flattenChoices (unwrapQuestion q)
    guard (length cs > 1)
    let
      code = maybe "00000" unInvestigatorId (resolveMlInvestigator pid game)
      sit = runIdentity (runReaderT (gatherSituation (defaultAiPlayerState code) pid) game)
      rowsValue =
        Array
          $ V.fromList
            [ object
                [ "chosen" .= (i == qrChoice)
                , "features" .= object [K.fromText name .= v | (name, v) <- choiceFeatures sit ui]
                , "breakdown" .= object [K.fromText name .= v | (name, v) <- scoreBreakdown sit ui]
                ]
            | (i, ui) <- zip [0 :: Int ..] cs
            ]
    pure (qrChoice, rowsValue)
  _ -> Nothing

{- | The seat's controlled investigator id in the decision snapshot (only its
card code is needed, to pick the AI focus profile). Mirrors arkham-extract.
-}
resolveMlInvestigator :: PlayerId -> Game -> Maybe InvestigatorId
resolveMlInvestigator pid game =
  toId <$> find ((== pid) . attr investigatorPlayerId) (Map.elems (gameEntities game).investigators)
