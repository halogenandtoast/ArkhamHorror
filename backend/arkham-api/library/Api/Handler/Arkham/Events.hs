{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Epic Multiplayer HTTP + websocket handlers.

An "event" owns N group games (each an ordinary 'ArkhamGame', reached through
the existing @/games/:id@ endpoints) plus the shared state. Milestone 1 wires
creation, a read-only dashboard payload, the per-event websocket feed, and a
single shared counter (countermeasures) adjustable by any member.
-}
module Api.Handler.Arkham.Events (
  getApiV1ArkhamEventsR,
  postApiV1ArkhamEventsR,
  getApiV1ArkhamEventR,
  postApiV1ArkhamEventCounterR,
) where

import Api.Arkham.Epic (applyEpicDeltasLocked)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Api.Handler.Arkham.Games.Shared (publishToEventRoom, publishToRoom, streamRoom)
import Arkham.Classes.HasQueue (newQueue)
import Arkham.Difficulty (Difficulty)
import Arkham.Epic.Types
import Arkham.Game (addPlayer, gameGameState, newScenario, runMessages)
import Arkham.Game.State (GameState)
import Arkham.Id (PlayerId (..), ScenarioId)
import Arkham.Queue (queueToRef)
import Control.Concurrent.MVar (modifyMVar_)
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Persist qualified as P
import Entity.Arkham.Step (ActionDiff (..), ArkhamStep (..), Choice (..))
import Import hiding (on, (==.))
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Yesod.WebSockets (WebSocketsT, webSockets)

-- Request bodies --------------------------------------------------------------

data CreateEventGroupPost = CreateEventGroupPost
  { name :: Text
  , playerCount :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data CreateEventPost = CreateEventPost
  { name :: Text
  , scenarioId :: ScenarioId
  , difficulty :: Difficulty
  , includeTarotReadings :: Bool
  , groups :: [CreateEventGroupPost]
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data CounterPost = CounterPost
  { key :: Text
  , amount :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- Response payloads -----------------------------------------------------------

data GroupDigest = GroupDigest
  { ordinal :: Int
  , name :: Text
  , gameId :: Maybe ArkhamGameId
  , gameState :: Maybe GameState
  , investigatorCount :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data EventDetails = EventDetails
  { id :: ArkhamEpicEventId
  , name :: Text
  , organizerUserId :: UserId
  , role :: Maybe EpicRole
  , sharedState :: SharedEventState
  , totalInvestigators :: Int
  , groups :: [GroupDigest]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data EventListEntry = EventListEntry
  { id :: ArkhamEpicEventId
  , name :: Text
  , role :: EpicRole
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- AuthZ -----------------------------------------------------------------------

requireEventMember :: UserId -> ArkhamEpicEventId -> Handler EpicRole
requireEventMember userId eid = do
  mMember <-
    runDB
      $ P.selectFirst
        [ArkhamEpicMemberArkhamEpicEventId P.==. eid, ArkhamEpicMemberUserId P.==. userId]
        []
  case mMember of
    Just (Entity _ m) -> pure (arkhamEpicMemberRole m)
    Nothing -> permissionDenied "You are not a member of this event"

-- Handlers --------------------------------------------------------------------

-- | List the events the current user is a member of.
getApiV1ArkhamEventsR :: Handler [EventListEntry]
getApiV1ArkhamEventsR = do
  userId <- getRequestUserId
  rows <- runDB $ select do
    (member :& event) <-
      from
        $ table @ArkhamEpicMember
        `innerJoin` table @ArkhamEpicEvent
          `on` (\(member :& event) -> member.arkhamEpicEventId ==. event.id)
    where_ $ member.userId ==. val userId
    orderBy [desc event.updatedAt]
    pure (event.id, event.name, member.role)
  pure
    [ EventListEntry {id = eid, name = nm, role = r}
    | (Value eid, Value nm, Value r) <- rows
    ]

{- | Create an event: build N group games (each a normal scenario game) and the
event aggregate, seeding shared countermeasures = ceil(totalInvestigators/2).
The creator becomes the organizer.
-}
postApiV1ArkhamEventsR :: Handler EventDetails
postApiV1ArkhamEventsR = do
  userId <- getRequestUserId
  CreateEventPost {..} <- requireCheckJsonBody
  now <- liftIO getCurrentTime

  let
    totalInvestigators = sum (map (.playerCount) groups)
    countermeasures = (totalInvestigators + 1) `div` 2
    seeded = setSharedCounter Countermeasures countermeasures (emptySharedEventState totalInvestigators)

  -- Create each group's game up front (own transaction per game, mirroring the
  -- normal game-creation path).
  groupGames <- for (zip [0 :: Int ..] groups) \(ordx, grp) -> do
    gid <- createGroupGame userId grp.name scenarioId difficulty includeTarotReadings grp.playerCount
    pure (ordx, grp, gid)

  eid <- runDB do
    eid <-
      P.insert
        $ ArkhamEpicEvent
          name
          userId
          (Just (tshow scenarioId))
          Nothing
          (tshow difficulty)
          seeded
          totalInvestigators
          0
          now
          now
    P.insert_ $ ArkhamEpicMember eid userId Organizer Nothing
    for_ groupGames \(ordx, grp, gid) ->
      P.insert_ $ ArkhamEpicGroup eid ordx (Just gid) grp.name grp.playerCount
    pure eid

  buildEventDetails userId eid

-- | Dashboard payload. Upgrades to the event websocket feed when requested.
getApiV1ArkhamEventR :: ArkhamEpicEventId -> Handler EventDetails
getApiV1ArkhamEventR eid = do
  userId <- getRequestUserId
  webSockets $ eventStream eid
  void $ requireEventMember userId eid
  buildEventDetails userId eid

{- | Adjust a shared counter. Any member may do so; the mutation is recorded as a
delta on the locked event row and broadcast to the event feed and every
group's own game stream.
-}
postApiV1ArkhamEventCounterR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventCounterR eid = do
  userId <- getRequestUserId
  void $ requireEventMember userId eid
  CounterPost {..} <- requireCheckJsonBody
  case sharedKeyFromText key of
    Nothing -> invalidArgs ["Unknown shared key: " <> key]
    Just sharedKey -> do
      did <- UUID.toText <$> liftIO nextRandom
      let delta = SharedDelta {sharedDeltaId = did, sharedDeltaKey = sharedKey, sharedDeltaAmount = amount}
      newState <- runDB $ applyEpicDeltasLocked eid Nothing Nothing [delta]
      publishToEventRoom eid (SharedStateUpdate newState)
      gameIds <- runDB $ select do
        grp <- from $ table @ArkhamEpicGroup
        where_ $ grp.arkhamEpicEventId ==. val eid
        pure grp.arkhamGameId
      for_ gameIds \(Value mGid) ->
        for_ mGid \gid -> publishToRoom gid (SharedStateUpdate newState)

-- Helpers ---------------------------------------------------------------------

buildEventDetails :: UserId -> ArkhamEpicEventId -> Handler EventDetails
buildEventDetails userId eid = do
  mEvent <- runDB $ P.get eid
  case mEvent of
    Nothing -> notFound
    Just event -> do
      groupRows <- runDB $ select do
        grp <- from $ table @ArkhamEpicGroup
        where_ $ grp.arkhamEpicEventId ==. val eid
        orderBy [asc grp.ordinal]
        pure grp
      mRole <-
        runDB
          $ P.selectFirst
            [ArkhamEpicMemberArkhamEpicEventId P.==. eid, ArkhamEpicMemberUserId P.==. userId]
            []
      digests <- traverse mkGroupDigest groupRows
      pure
        EventDetails
          { id = eid
          , name = arkhamEpicEventName event
          , organizerUserId = arkhamEpicEventOrganizerUserId event
          , role = arkhamEpicMemberRole . entityVal <$> mRole
          , sharedState = arkhamEpicEventSharedState event
          , totalInvestigators = arkhamEpicEventTotalInvestigators event
          , groups = digests
          }

mkGroupDigest :: Entity ArkhamEpicGroup -> Handler GroupDigest
mkGroupDigest (Entity _ grp) = case arkhamEpicGroupArkhamGameId grp of
  Nothing ->
    pure
      GroupDigest
        { ordinal = ordx
        , name = nm
        , gameId = Nothing
        , gameState = Nothing
        , investigatorCount = 0
        }
  Just gid -> do
    mGame <- runDB $ P.get gid
    playerCount <- runDB $ P.count [ArkhamPlayerArkhamGameId P.==. gid]
    pure
      GroupDigest
        { ordinal = ordx
        , name = nm
        , gameId = Just gid
        , gameState = gameGameState . arkhamGameCurrentData <$> mGame
        , investigatorCount = playerCount
        }
 where
  ordx = arkhamEpicGroupOrdinal grp
  nm = arkhamEpicGroupName grp

{- | Create one group's game using the normal scenario-creation path. The creator
holds every seat (Solo) so a single user can drive the group in Milestone 1.
-}
createGroupGame
  :: UserId -> Text -> ScenarioId -> Difficulty -> Bool -> Int -> Handler ArkhamGameId
createGroupGame userId gameName scenarioId difficulty includeTarotReadings playerCount = do
  newGameSeed <- liftIO getRandom
  genRef <- newIORef (mkStdGen newGameSeed)
  queueRef <- newQueue []
  now <- liftIO getCurrentTime
  tracer <- getTracer
  let
    seats = max 1 playerCount
    game = newScenario scenarioId newGameSeed seats difficulty includeTarotReadings
    ag = ArkhamGame gameName game 0 Solo now now
  runDB do
    gameId <- P.insert ag
    pids <- replicateM seats $ P.insert $ ArkhamPlayer userId gameId "00000"
    gameRef <- liftIO $ newIORef game
    runGameApp (GameApp gameRef queueRef genRef (pure . const ()) tracer Nothing) do
      for_ pids \pid -> addPlayer (PlayerId $ coerce pid)
      runMessages (gameIdToText gameId) Nothing
    updatedGame <- liftIO $ readIORef gameRef
    updatedQueue <- liftIO $ readIORef (queueToRef queueRef)
    let ag' = ag {arkhamGameCurrentData = updatedGame}
    P.replace gameId ag'
    P.insert_ $ ArkhamStep gameId (Choice mempty updatedQueue) 0 (ActionDiff [])
    pure gameId

-- | The per-event websocket: a read-only feed of shared-state updates.
eventStream :: ArkhamEpicEventId -> WebSocketsT Handler ()
eventStream eid = do
  room <- lift $ getEventRoom eid
  streamRoom (eventChannel eid) room do
    roomsVar <- lift $ getsYesod appEventRooms
    liftIO $ modifyMVar_ roomsVar $ pure . Map.delete eid
    lift $ removeChannel (eventChannel eid)
