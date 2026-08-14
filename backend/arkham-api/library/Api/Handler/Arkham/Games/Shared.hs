{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games.Shared where

import Api.Arkham.Epic (
  applyEpicDeltasLocked,
  lookupGameEvent,
  mkEpicEnv,
  modifySharedStateLockedWith,
 )
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Achievement.Types (Achievement, achievementChecklist, achievementName)
import Arkham.Asset.Types (Asset, assetController, assetOwner, assetPlacement)
import Arkham.Campaign.Types (CampaignAttrs)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.Card.CardCode (CardCode (..), HasCardCode (toCardCode))
import Arkham.ClassSymbol
import Arkham.Classes.Entity (attr, overAttrs, toAttrs)
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Difficulty
import Arkham.Effect.Types (effectTarget)
import Arkham.Entities (Entities (..), entitiesActs)
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedEventState,
  SharedKey (
    ActAdvanceGen,
    ActContribution,
    ActSpend,
    AwaitingOrganizer,
    MainStreetEligible,
    MainStreetReady,
    SharedActProgress
  ),
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
import Arkham.Event.Types (eventController)
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State
import Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlacement, investigatorPlayerId)
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Name
import Arkham.Placement (
  Placement (AtLocation, AttachedToInvestigator, InPlayArea, InThreatArea, StillInHand),
 )
import Arkham.Queue
import Arkham.Scenario.Types (Scenario, getMetaKeyDefault)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Target (Target (InvestigatorTarget))
import Arkham.Treachery.Types (treacheryPlacement)
import Conduit
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Data.Traversable (for)
import Data.UUID (nil)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Redis (RedisChannel, msgMessage, pubSub, publish, runRedis, subscribe)
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.), (>=.))
import Import qualified as P
import Json
import Network.WebSockets (ConnectionException, withPingThread)
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception hiding (Handler)
import UnliftIO.Timeout (timeout)
import Yesod.WebSockets

{- | How often to ping an idle websocket. Must stay comfortably under Warp's
'settingsTimeout' (30s by default) -- see 'withKeepAlive'.
-}
keepAlivePingSeconds :: Int
keepAlivePingSeconds = 15

{- | Warp treats a websocket as a raw response and only tickles its idle
timeout on real socket traffic, so a quiet game (nobody taking a turn) is
torn down after 'settingsTimeout' seconds and the client silently
reconnects -- a 30s churn cycle per open tab. A server-side ping well inside
that window keeps the socket alive, and does the same for any proxy in front
of it (the Vite dev proxy in development, nginx/CloudFront in production).
-}
withKeepAlive :: WebSocketsT Handler a -> WebSocketsT Handler a
withKeepAlive inner = do
  conn <- ask
  withRunInIO \run -> withPingThread conn keepAlivePingSeconds (pure ()) (run inner)

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream gameId = catchingConnectionException $ withKeepAlive do
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
streamRoom channel room onLastLeave = catchingConnectionException $ withKeepAlive do
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
  , variant :: Maybe Text
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

data EpicOrganizerGateBlocked = EpicOrganizerGateBlocked
  deriving stock Show
  deriving anyclass Exception

updateGame :: Answer -> ArkhamGameId -> Maybe Room -> Handler ()
updateGame response gameId mRoom = do
  let broadcast :: Broadcast
      broadcast = case mRoom of
        Nothing -> \_ -> pure ()
        Just room -> broadcastToRoom room
  let rejectOrganizerGate action =
        action `catch` \EpicOrganizerGateBlocked ->
          permissionDenied "This event is waiting for the organizer's clue allocation"
  (ArkhamGame {..}, oldLogEntries, updatedLog, mSharedUpdate, actAdvanced, newAchievements) <- rejectOrganizerGate $ runDB $ atomicallyWithGame gameId \g@ArkhamGame {..} -> do
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

    logRef <- newIORef []
    reply <- handleAnswer gameJson playerId response
    case reply of
      Unhandled _ -> pure (g, oldLogEntries, [], Nothing, False, [])
      Handled answerMessages -> do
        -- Epic Multiplayer: if this game is a group within an event, build an
        -- EpicEnv so Shared* messages emitted during the action are captured as
        -- deltas. 'Nothing' (every ordinary game) means zero behavior change.
        mEpicCtx <- lookupGameEvent gameId
        -- The organizer barrier is a server-side lock, not merely a frontend
        -- overlay. This also closes direct-API and delayed-click paths that could
        -- otherwise answer the parked Continue before allocation.
        for_ mEpicCtx \(Entity _ event, _) -> do
          let
            shared = arkhamEpicEventSharedState event
            gateOpen = any (\stage -> sharedCounter (AwaitingOrganizer stage) shared > 0) (actProgressStages shared)
            continuesActAdvance = any (\case NextAdvanceActStep {} -> True; _ -> False) answerMessages
          when (gateOpen && continuesActAdvance) $ liftIO $ throwIO EpicOrganizerGateBlocked
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
        -- Above-the-table achievements: collect EarnAchievement messages via
        -- the (otherwise unused) runMessages message logger; persisted below.
        achievementsRef <- newIORef []
        achievementProgressRef <- newIORef []
        let
          collectAchievements = \case
            EarnAchievement a -> modifyIORef' achievementsRef (a :)
            AchievementProgress a items -> modifyIORef' achievementProgressRef ((a, items) :)
            _ -> pure ()
        mResult <- liftIO $ timeout runMessagesTimeoutMicros do
          runGameApp (GameApp gameRef queueRef genRef (handleMessageLog logRef broadcast) mEpicEnv) do
            runMessages (gameIdToText gameId) (Just collectAchievements)
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

        -- Persist newly earned achievements: one row per human player per
        -- achievement, ever. insertUnique against UniqueUserAchievement makes
        -- re-earns no-ops; only genuinely new rows produce a toast.
        earned <- liftIO $ ordNub . reverse <$> readIORef achievementsRef
        -- Checklist progress (AchievementProgress): merge this action's items
        -- per achievement, then per user below.
        progressed <- liftIO $ reverse <$> readIORef achievementProgressRef
        let
          progressList =
            [ (a, ordNub $ concat [zs | (a', zs) <- progressed, a' == a])
            | a <- ordNub (map fst progressed)
            ]
        newAchievements <-
          if null earned && null progressList
            then pure []
            else do
              players <- P.selectList [ArkhamPlayerArkhamGameId P.==. gameId] []
              let userIds = ordNub $ map (arkhamPlayerUserId . entityVal) players
              directEarns <- fmap concat $ for earned \achievement -> do
                inserted <- for userIds \uid ->
                  P.insertUnique
                    $ ArkhamAchievement uid achievement (Just now) (Just gameId) Null
                pure [achievement | any isJust inserted]
              -- Cross-playthrough checklists: accumulate items in the row's
              -- progress column and earn when every checklist item is in.
              -- Each user has their own history, so completion is per user.
              progressEarns <- fmap concat $ for progressList \(achievement, items) -> do
                completions <- for userIds \uid ->
                  applyAchievementProgress uid achievement items gameId now
                pure [achievement | or completions]
              pure $ ordNub $ directEarns <> progressEarns

        pure (g', oldLogEntries, updatedLog, mSharedUpdate, actAdvanced, newAchievements)

  -- Update the per-room cache after the DB transaction has committed,
  -- so the cache is never ahead of durably-stored state.
  let publishLog = oldLogEntries <> updatedLog
  liftIO $ writeCachedLog mRoom arkhamGameStep publishLog

  -- Publish shared state before the acting game's parked question. In particular,
  -- a threshold-crossing Act 1 action has already armed AwaitingOrganizer in the
  -- same event-row write, so clients install the blocking overlay before they can
  -- see or answer the parked Continue question.
  -- Main Street's cross-game action is available only while investigators in at
  -- least two distinct groups are currently at their copies. Movement normally
  -- emits no shared delta, so derive this presence bit after every persisted
  -- action rather than relying on card messages.
  mMainStreetUpdate <- refreshMainStreetEligibility gameId
  case mMainStreetUpdate of
    Just (eid, s) -> propagateShared eid Nothing s
    Nothing -> for_ mSharedUpdate \(eid, s) -> propagateShared eid (Just gameId) s

  publishToRoom gameId
    $ GameUpdate
    $ PublicGame
      gameId
      arkhamGameName
      publishLog
      arkhamGameCurrentData

  -- Achievement unlock toasts, after the rows are durably committed.
  for_ newAchievements \achievement ->
    publishToRoom gameId $ GameAchievement (achievementName achievement)

  -- Epic Multiplayer: wall off undo across an IN-GROUP act advance. Each group
  -- advances its own act via the normal AdvanceAct flow (no cross-group injection);
  -- when this action advanced the act, set the per-game undo floor to the committed
  -- step so it can't be locally undone (the other groups follow on their own turns
  -- via 'ActAdvanceGen'). 'arkhamGameStep' here is the post-commit (new) step.
  when actAdvanced $ setGameUndoFloor gameId arkhamGameStep

{- | Merge reported checklist items into the user's progress row for a
cross-playthrough achievement (see 'achievementChecklist'); the row's
progress column holds the checked item keys as a JSON array. Returns True
when this merge completed the checklist — the row flips to earned, pointing
at the completing game. Already-earned rows are left untouched.
-}
applyAchievementProgress
  :: UserId -> Achievement -> [Text] -> ArkhamGameId -> UTCTime -> DB Bool
applyAchievementProgress uid achievement items gameId now = do
  let
    checklist = fromMaybe [] (achievementChecklist achievement)
    complete merged = not (null checklist) && all (`elem` merged) checklist
  P.getBy (UniqueUserAchievement uid achievement) >>= \case
    Just (Entity rowId row)
      | isJust (arkhamAchievementEarnedAt row) -> pure False
      | otherwise -> do
          let
            existing = case fromJSON (arkhamAchievementProgress row) of
              Success xs -> xs
              _ -> []
            merged = ordNub (existing <> items)
          if complete merged
            then do
              P.update
                rowId
                [ ArkhamAchievementProgress P.=. toJSON merged
                , ArkhamAchievementEarnedAt P.=. Just now
                , ArkhamAchievementArkhamGameId P.=. Just gameId
                ]
              pure True
            else do
              P.update rowId [ArkhamAchievementProgress P.=. toJSON merged]
              pure False
    Nothing -> do
      let merged = ordNub items
      if complete merged
        then do
          void
            $ P.insertUnique
            $ ArkhamAchievement uid achievement (Just now) (Just gameId) (toJSON merged)
          pure True
        else do
          void
            $ P.insertUnique
            $ ArkhamAchievement uid achievement Nothing Nothing (toJSON merged)
          pure False

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

-- Group roster payloads contain caller-specific fields (role/youAreSeated), so a
-- membership change broadcasts only an invalidation and each client refetches.
broadcastEventChanged :: ArkhamEpicEventId -> Handler ()
broadcastEventChanged eid = do
  publishToEventRoom eid EventChanged
  gameIds <- getEventGroupGameIds eid
  for_ gameIds (`publishToRoom` EventChanged)

refreshMainStreetEligibility
  :: ArkhamGameId -> Handler (Maybe (ArkhamEpicEventId, SharedEventState))
refreshMainStreetEligibility gameId = do
  mEvent <- runDB $ lookupGameEvent gameId
  case mEvent of
    Nothing -> pure Nothing
    Just (Entity eid _, _) -> do
      groups <- runDB $ P.selectList [ArkhamEpicGroupArkhamEpicEventId P.==. eid] []
      let gameIds = mapMaybe (arkhamEpicGroupArkhamGameId . entityVal) groups
      games <- runDB $ traverse P.getJust gameIds
      let
        groupAtMainStreet rawGame =
          let
            game = arkhamGameCurrentData rawGame
            mainStreets =
              Map.keysSet
                $ Map.filter
                  ((== toCardCode Locations.mainStreet) . toCardCode)
                  (entitiesLocations $ gameEntities game)
           in
            any
              ( \investigator -> case attr investigatorPlacement investigator of
                  AtLocation lid -> lid `Set.member` mainStreets
                  _ -> False
              )
              (entitiesInvestigators $ gameEntities game)
        eligible = length (filter groupAtMainStreet games) >= 2
      (shared, changed) <- runDB $ modifySharedStateLockedWith eid \s ->
        let value = if eligible then 1 else 0
         in if sharedCounter MainStreetEligible s == value
              then (s, False)
              else (setSharedCounter MainStreetEligible value s, True)
      pure $ (eid, shared) <$ guard changed

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
          $ runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing)
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

{- | Resolve the ELSE! Main Street group swap. InvestigatorAttrs contains the
investigator's deck, hand, discard, resources, damage, trauma, logs, and
other personal state. We additionally move every controlled/play-area asset,
threat-area treachery, controlled event, per-investigator entity cache,
history, question, and player authorization row. Enemy cards stay behind
because the printed ability disengages them before publishing readiness.

This writes both games in one transaction, advances both revisions, publishes
both websocket rooms, and places an undo floor at the new revisions. A swap
is therefore never half-visible and can never be crossed by ordinary undo.
-}
swapMainStreetInvestigators :: ArkhamEpicEventId -> Int -> Int -> Handler ()
swapMainStreetInvestigators eventId firstOrdinal secondOrdinal = do
  (firstGameId, secondGameId) <- runDB do
    groups <-
      P.selectList
        [ ArkhamEpicGroupArkhamEpicEventId P.==. eventId
        , ArkhamEpicGroupOrdinal P.<-. [firstOrdinal, secondOrdinal]
        ]
        []
    let byOrdinal =
          Map.fromList
            [ (arkhamEpicGroupOrdinal g, gid)
            | Entity _ g <- groups
            , gid <- toList (arkhamEpicGroupArkhamGameId g)
            ]
    (,)
      <$> maybe
        (error "First Main Street group has no game")
        pure
        (Map.lookup firstOrdinal byOrdinal)
      <*> maybe
        (error "Second Main Street group has no game")
        pure
        (Map.lookup secondOrdinal byOrdinal)

  runDB do
    firstRaw <- P.getJust firstGameId
    secondRaw <- P.getJust secondGameId
    let
      firstGame = arkhamGameCurrentData firstRaw
      secondGame = arkhamGameCurrentData secondRaw
      scenarioReady :: Scenario -> Maybe InvestigatorId
      scenarioReady scenario = getMetaKeyDefault "mainStreetReady" Nothing (toAttrs scenario)
      readyInvestigator :: Game -> Maybe InvestigatorId
      readyInvestigator game = case gameMode game of
        That scenario -> scenarioReady scenario
        These _ scenario -> scenarioReady scenario
        This _ -> Nothing
      firstIid = fromMaybe (error "First group has not activated Main Street") $ readyInvestigator firstGame
      secondIid = fromMaybe (error "Second group has not activated Main Street") $ readyInvestigator secondGame
      mainStreetLocation game =
        fromMaybe (error "Ready investigator is not at Main Street")
          $ (.id)
          <$> find ((== CardCode "89006") . toCardCode) (toList $ entitiesLocations $ gameEntities game)
      firstDestination = mainStreetLocation secondGame
      secondDestination = mainStreetLocation firstGame
      (firstGame', secondGame', firstPid, secondPid) =
        swapInvestigatorState firstIid firstDestination firstGame secondIid secondDestination secondGame
      firstStep = arkhamGameStep firstRaw + 1
      secondStep = arkhamGameStep secondRaw + 1
    P.update firstGameId [ArkhamGameCurrentData P.=. firstGame', ArkhamGameStep P.=. firstStep]
    P.update secondGameId [ArkhamGameCurrentData P.=. secondGame', ArkhamGameStep P.=. secondStep]
    P.update (coerce firstPid) [ArkhamPlayerArkhamGameId P.=. secondGameId]
    P.update (coerce secondPid) [ArkhamPlayerArkhamGameId P.=. firstGameId]

  runMessagesInGroupWhen
    (const True)
    [SpendShared (MainStreetReady $ GroupOrdinal firstOrdinal) 1]
    firstGameId
  runMessagesInGroupWhen
    (const True)
    [SpendShared (MainStreetReady $ GroupOrdinal secondOrdinal) 1]
    secondGameId
  for_ [firstGameId, secondGameId] \gameId -> do
    raw <- runDB $ P.get404 gameId
    setGameUndoFloor gameId (arkhamGameStep raw)
    publishToRoom gameId
      $ GameUpdate
      $ PublicGame gameId (arkhamGameName raw) [] (arkhamGameCurrentData raw)
  broadcastEventChanged eventId

swapInvestigatorState
  :: InvestigatorId
  -> LocationId
  -> Game
  -> InvestigatorId
  -> LocationId
  -> Game
  -> (Game, Game, PlayerId, PlayerId)
swapInvestigatorState firstIid firstDestination firstGame secondIid secondDestination secondGame
  | attr investigatorPlacement firstInvestigator /= AtLocation secondDestination =
      error "First ready investigator is no longer at Main Street"
  | attr investigatorPlacement secondInvestigator /= AtLocation firstDestination =
      error "Second ready investigator is no longer at Main Street"
  | otherwise =
      ( install secondIid secondMoved secondOwned secondPid (remove firstIid firstPid firstGame)
      , install firstIid firstMoved firstOwned firstPid (remove secondIid secondPid secondGame)
      , firstPid
      , secondPid
      )
 where
  firstInvestigator =
    fromMaybe (error "First Main Street investigator is not in its game")
      $ Map.lookup firstIid (entitiesInvestigators $ gameEntities firstGame)
  secondInvestigator =
    fromMaybe (error "Second Main Street investigator is not in its game")
      $ Map.lookup secondIid (entitiesInvestigators $ gameEntities secondGame)
  firstPid = attr investigatorPlayerId firstInvestigator
  secondPid = attr investigatorPlayerId secondInvestigator
  firstMoved = overAttrs (\a -> a {investigatorPlacement = AtLocation secondDestination}) firstInvestigator
  secondMoved = overAttrs (\a -> a {investigatorPlacement = AtLocation firstDestination}) secondInvestigator
  firstOwned = ownedEntities firstIid (gameEntities firstGame)
  secondOwned = ownedEntities secondIid (gameEntities secondGame)

  remove iid pid game =
    game
      { gameEntities = removeOwned iid (gameEntities game)
      , gamePlayers = filter (/= pid) (gamePlayers game)
      , gamePlayerOrder = filter (/= iid) (gamePlayerOrder game)
      , gameInHandEntities = Map.delete iid (gameInHandEntities game)
      , gameInDiscardEntities = Map.delete iid (gameInDiscardEntities game)
      , gamePhaseHistory = Map.delete iid (gamePhaseHistory game)
      , gameTurnHistory = Map.delete iid (gameTurnHistory game)
      , gameRoundHistory = Map.delete iid (gameRoundHistory game)
      , gameQuestion = Map.delete pid (gameQuestion game)
      , gameModifiers = Map.delete (InvestigatorTarget iid) (gameModifiers game)
      , gameCardUses = Map.map (filter (/= iid)) (gameCardUses game)
      }

  install iid investigator owned pid game =
    game
      { gameEntities = addOwned investigator owned (gameEntities game)
      , gamePlayers = gamePlayers game <> [pid]
      , gamePlayerOrder = gamePlayerOrder game <> [iid]
      , gameInHandEntities =
          copyMapEntry
            iid
            (if iid == firstIid then gameInHandEntities firstGame else gameInHandEntities secondGame)
            (gameInHandEntities game)
      , gameInDiscardEntities =
          copyMapEntry
            iid
            (if iid == firstIid then gameInDiscardEntities firstGame else gameInDiscardEntities secondGame)
            (gameInDiscardEntities game)
      , gamePhaseHistory =
          copyMapEntry
            iid
            (if iid == firstIid then gamePhaseHistory firstGame else gamePhaseHistory secondGame)
            (gamePhaseHistory game)
      , gameTurnHistory =
          copyMapEntry
            iid
            (if iid == firstIid then gameTurnHistory firstGame else gameTurnHistory secondGame)
            (gameTurnHistory game)
      , gameRoundHistory =
          copyMapEntry
            iid
            (if iid == firstIid then gameRoundHistory firstGame else gameRoundHistory secondGame)
            (gameRoundHistory game)
      , gameQuestion =
          copyMapEntry
            pid
            (if pid == firstPid then gameQuestion firstGame else gameQuestion secondGame)
            (gameQuestion game)
      , gameModifiers =
          copyMapEntry
            (InvestigatorTarget iid)
            (if iid == firstIid then gameModifiers firstGame else gameModifiers secondGame)
            (gameModifiers game)
      , gameCardUses =
          transferCardUses
            iid
            (if iid == firstIid then gameCardUses firstGame else gameCardUses secondGame)
            (gameCardUses game)
      , gameActiveInvestigatorId = replaceId firstIid secondIid iid (gameActiveInvestigatorId game)
      , gameTurnPlayerInvestigatorId =
          replaceId firstIid secondIid iid <$> gameTurnPlayerInvestigatorId game
      , gameLeadInvestigatorId = replaceId firstIid secondIid iid (gameLeadInvestigatorId game)
      , gameActivePlayerId =
          if gameActivePlayerId game `elem` [firstPid, secondPid] then pid else gameActivePlayerId game
      }

  replaceId removedA removedB inserted current
    | current == removedA || current == removedB = inserted
    | otherwise = current

transferCardUses
  :: InvestigatorId
  -> Map CardCode [InvestigatorId]
  -> Map CardCode [InvestigatorId]
  -> Map CardCode [InvestigatorId]
transferCardUses iid source destination =
  Map.unionWith
    (<>)
    (Map.map (const [iid]) $ Map.filter (elem iid) source)
    (Map.map (filter (/= iid)) destination)

copyMapEntry :: Ord key => key -> Map key value -> Map key value -> Map key value
copyMapEntry key source destination = maybe destination (\value -> Map.insert key value destination) (Map.lookup key source)

ownedEntities :: InvestigatorId -> Entities -> Entities
ownedEntities iid entities =
  mempty
    { entitiesAssets = Map.filter (assetBelongsTo iid) (entitiesAssets entities)
    , entitiesTreacheries =
        Map.filter
          ((`elem` [InThreatArea iid, AttachedToInvestigator iid]) . attr treacheryPlacement)
          (entitiesTreacheries entities)
    , entitiesEvents = Map.filter ((== iid) . attr eventController) (entitiesEvents entities)
    , entitiesEffects =
        Map.filter ((== InvestigatorTarget iid) . attr effectTarget) (entitiesEffects entities)
    }

removeOwned :: InvestigatorId -> Entities -> Entities
removeOwned iid entities =
  entities
    { entitiesInvestigators = Map.delete iid (entitiesInvestigators entities)
    , entitiesAssets = Map.filter (not . assetBelongsTo iid) (entitiesAssets entities)
    , entitiesTreacheries =
        Map.filter
          (not . (`elem` [InThreatArea iid, AttachedToInvestigator iid]) . attr treacheryPlacement)
          (entitiesTreacheries entities)
    , entitiesEvents = Map.filter ((/= iid) . attr eventController) (entitiesEvents entities)
    , entitiesEffects =
        Map.filter ((/= InvestigatorTarget iid) . attr effectTarget) (entitiesEffects entities)
    }

addOwned :: Investigator -> Entities -> Entities -> Entities
addOwned investigator owned entities =
  entities
    { entitiesInvestigators = Map.insert investigator.id investigator (entitiesInvestigators entities)
    , entitiesAssets = entitiesAssets owned <> entitiesAssets entities
    , entitiesTreacheries = entitiesTreacheries owned <> entitiesTreacheries entities
    , entitiesEvents = entitiesEvents owned <> entitiesEvents entities
    , entitiesEffects = entitiesEffects owned <> entitiesEffects entities
    }

assetBelongsTo :: InvestigatorId -> Asset -> Bool
assetBelongsTo iid asset =
  attr assetController asset
    == Just iid
    || attr assetOwner asset
    == Just iid
    || attr assetPlacement asset
    `elem` [InPlayArea iid, InThreatArea iid, StillInHand iid, AttachedToInvestigator iid]

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

{- | Floor undo for EVERY group in the event at its CURRENT persistence step,
making a consuming act advance a global checkpoint: no group can undo across it (so
no contributor can rewind a now-consumed pool placement), while every group's
actions AFTER it stay undoable. Floors are monotonic (always set at a later step
than any prior floor). Called only after the organizer consumes the pool in
'settleOrganizerAdvance'.
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

{- | Each group's @(ordinal, contribution)@ toward a stage-@stage@ advance, read
from the authoritative shared 'ActContribution' counters (mirrored from the
contributing acts). Shaped for the organizer endpoint to cap each group's spend.
-}
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
      pure
        [ (ordinal, sharedCounter (ActContribution stage (GroupOrdinal ordinal)) shared)
        | (ordinal, _gid) <- groups
        ]

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
                That s ->
                  Just
                    $ ScenarioDetails
                      s.id
                      s.difficulty
                      s.name
                      (getMetaKeyDefault "variant" Nothing $ toAttrs s)
                These _ s ->
                  Just
                    $ ScenarioDetails
                      s.id
                      s.difficulty
                      s.name
                      (getMetaKeyDefault "variant" Nothing $ toAttrs s)
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
