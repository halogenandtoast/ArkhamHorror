{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games.Shared where

import Api.Arkham.Epic (applyEpicDeltasLocked, lookupGameEvent, mkEpicEnv)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Ai.Decision (decideAi, decideAiAssist, isAssistCommitWindow)
import Arkham.Ai.Helpers (lookupAiPlayer)
import Arkham.Ai.State (aiEnabled)
import Arkham.Epic.Types (SharedEventState, epicEnvDeltaRef, epicEnvSharedRef, sharedCounters, sharedTotalInvestigators)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Campaign.Types (CampaignAttrs)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.ClassSymbol
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Difficulty
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State
import Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator)
import Arkham.Message
import Arkham.Name
import Arkham.Queue
import Conduit
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Lens (view)
import Data.IntMap.Strict qualified as IntMap
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Data.UUID (nil)
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

-- | Generic read-only room subscription loop: bridge the Redis channel into the
-- room, fan room messages out to this websocket, and run @onLastLeave@ when the
-- final subscriber disconnects. Inbound client frames are ignored (read-only).
-- Used by the Epic Multiplayer event stream; 'gameStream' has its own variant
-- with per-game member counting and a log cache.
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

-- | A broadcast callback. Used to fan out log lines and game-state updates
-- to every WebSocket subscriber on a room. May be a no-op if there are no
-- subscribers (e.g. a direct REST PUT with no client listening), in which
-- case messages are silently dropped instead of buffered indefinitely.
type Broadcast = BSL.ByteString -> IO ()

-- | Hard cap on a single runMessages invocation. If a game's message
-- processing exceeds this we kill the action and roll back the surrounding
-- DB transaction so the worker (and the FOR UPDATE lock on the game row)
-- can be released. Empirically a normal action completes in well under 1s;
-- 30s gives plenty of headroom for slow-but-legitimate scenario setup
-- while still preventing one poison game from monopolising a worker.
runMessagesTimeoutMicros :: Int
runMessagesTimeoutMicros = 30 * 1000000

-- | Thrown by updateGame when 'runMessages' exceeds 'runMessagesTimeoutMicros'.
-- The Yesod handler turns this into a 500; the important effect is that the
-- exception propagates out of runDB, rolls back the transaction, and frees
-- the worker. Search Honeycomb / logs for this to find poison games.
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
  -- NOTE: not wrapping the whole handler in withSpan_ -- it would rewrap
  -- Yesod's HCContent control-flow exceptions (notFound, notAuthenticated,
  -- sendStatusJSON, etc.) and break 404/401 responses (see Undo.hs note).
  -- The runMessages span below is wrapped where it's safe.
  (ArkhamGame {..}, oldLogEntries, updatedLog, mSharedUpdate) <- runDB $ atomicallyWithGame gameId \g@ArkhamGame {..} -> do
    -- Read the prior log from the per-room cache when it's in sync with
    -- the just-locked game's step; otherwise fall back to the DB. Avoids
    -- the 217-row-avg getGameLog read on every action in the common case.
    oldLogEntries <- liftIO (lookupCachedLog mRoom arkhamGameStep) >>= \case
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
      Unhandled _ -> pure (g, oldLogEntries, [], Nothing)
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
        epicSyncMessages <- case mEpicEnv of
          Nothing -> pure []
          Just epic -> do
            shared <- liftIO $ readIORef (epicEnvSharedRef epic)
            pure
              $ [ ScenarioCountSet (EpicShared k) v
                | (k, v) <- Map.toList (sharedCounters shared)
                ]
              <> [ScenarioCountSet (EpicShared "total-investigators") (sharedTotalInvestigators shared)]

        let
          messages =
            [SetActivePlayer playerId | activePlayer /= playerId]
              <> answerMessages
              <> [SetActivePlayer activePlayer | activePlayer /= playerId]
        gameRef <- newIORef gameJson
        queueRef <- newQueue ((ClearUI : epicSyncMessages <> messages) <> currentQueue)
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
            ( ArkhamStep gameId
                (Choice diffDown updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL ge)
            )
            [ ArkhamStepChoice =. Choice diffDown updatedQueue
            , ArkhamStepActionDiff =. ActionDiff (view actionDiffL ge)
            ]

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

        pure (g', oldLogEntries, updatedLog, mSharedUpdate)

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

  -- Epic Multiplayer: a shared-counter change in one group is broadcast to the
  -- whole event (dashboard feed + every group's stream) so all connected clients
  -- reflect the new shared counters live. (Each group's board entities still
  -- re-sync from the counters on that group's next action.)
  for_ mSharedUpdate \(eid, s) -> broadcastSharedToEvent eid s

-- | Read the cached log entries IF the cache is consistent with the locked
-- game's current step. Returns Nothing on a mismatch (so the caller refetches
-- from the DB and refreshes the cache).
lookupCachedLog :: Maybe Room -> Int -> IO (Maybe [Text])
lookupCachedLog Nothing _ = pure Nothing
lookupCachedLog (Just room) currentStep = atomically do
  cachedVal <- readTVar (roomLogCache room)
  pure $ case cachedVal of
    Just c | c.cacheStep == currentStep -> Just c.cacheEntries
    _ -> Nothing

-- | Write the cache after a successful update. The step recorded is the new
-- post-update step; the next action will read the game at that step and find
-- a consistent cache.
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

-- | Broadcast a shared-state update to the event's dashboard feed AND to every
-- group's own game stream, so all connected clients (organizer dashboard,
-- organizer bars, shared displays) reflect the new shared counters live.
broadcastSharedToEvent :: ArkhamEpicEventId -> SharedEventState -> Handler ()
broadcastSharedToEvent eid s = do
  publishToEventRoom eid (SharedStateUpdate s)
  gameIds <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    pure grp.arkhamGameId
  for_ gameIds \(Value mGid) -> for_ mGid \gid -> publishToRoom gid (SharedStateUpdate s)

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
