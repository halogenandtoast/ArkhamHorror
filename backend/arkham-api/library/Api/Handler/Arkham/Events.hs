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
  deleteApiV1ArkhamEventR,
  postApiV1ArkhamEventCounterR,
) where

import Api.Arkham.Epic (applyEpicDeltasLocked)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Games.Shared (deleteEventRoom, deleteRoom, publishToEventRoom, publishToRoom, streamRoom)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Difficulty (Difficulty)
import Arkham.Epic.Types
import Arkham.Classes.Entity (attr)
import Arkham.Game (gameGameState, newScenario, setInitialScenarioMeta)
import Arkham.Game.State (GameState)
import Arkham.Game.Utils (gameInvestigators)
import Arkham.Id (InvestigatorId, PlayerId (..), ScenarioId)
import Arkham.Investigator.Types (Investigator, investigatorPlayerId)
import Control.Concurrent.MVar (modifyMVar_)
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

data GroupPlayerInfo = GroupPlayerInfo
  { username :: Text
  , investigatorId :: Maybe InvestigatorId
  -- ^ Nothing until the player has chosen an investigator.
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GroupDigest = GroupDigest
  { ordinal :: Int
  , name :: Text
  , gameId :: Maybe ArkhamGameId
  , gameState :: Maybe GameState
  , investigatorCount :: Int
  -- ^ players currently seated in this group
  , seatCount :: Int
  -- ^ total seats; investigatorCount < seatCount means the lobby has open seats
  , youAreSeated :: Bool
  -- ^ whether the requesting user holds a seat in this group (so an organizer who
  -- also plays can drop into it).
  , players :: [GroupPlayerInfo]
  -- ^ seated players (username + chosen investigator) for the dashboard.
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

-- | A user may hold both Organizer and GroupPlayer rows, so check for an
-- Organizer row directly rather than trusting the first membership found.
requireOrganizer :: UserId -> ArkhamEpicEventId -> Handler ()
requireOrganizer userId eid = do
  isOrganizer <-
    runDB
      $ P.exists
        [ ArkhamEpicMemberArkhamEpicEventId P.==. eid
        , ArkhamEpicMemberUserId P.==. userId
        , ArkhamEpicMemberRole P.==. Organizer
        ]
  unless isOrganizer $ permissionDenied "Only the organizer can delete this event"

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
    seeded =
      foldr
        (\(k, v) -> setSharedCounter k v)
        (emptySharedEventState totalInvestigators)
        (epicScenarioSeeds scenarioId totalInvestigators)

  -- Create each group's game up front (own transaction per game, mirroring the
  -- normal game-creation path).
  groupGames <- for (zip [0 :: Int ..] groups) \(ordx, grp) -> do
    gid <- createGroupGame grp.name scenarioId difficulty includeTarotReadings grp.playerCount
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

-- | Delete an event and all of its group games (organizer only). Deleting each
-- group's 'ArkhamGame' cascades its players/steps/logs and the epic-group row;
-- deleting the event cascades members and shared-state steps.
deleteApiV1ArkhamEventR :: ArkhamEpicEventId -> Handler ()
deleteApiV1ArkhamEventR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  gameValues <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    pure grp.arkhamGameId
  let gameIds = [gid | Value (Just gid) <- gameValues]
  runDB do
    for_ gameIds P.delete
    P.delete eid
  for_ gameIds deleteRoom
  deleteEventRoom eid

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
      -- A user may hold both Organizer and GroupPlayer rows (an organizer who
      -- also took a seat). Report Organizer in that case so organizer-only UI
      -- (e.g. the delete control) stays available.
      isOrganizer <-
        runDB
          $ P.exists
            [ ArkhamEpicMemberArkhamEpicEventId P.==. eid
            , ArkhamEpicMemberUserId P.==. userId
            , ArkhamEpicMemberRole P.==. Organizer
            ]
      mRole <-
        runDB
          $ P.selectFirst
            [ArkhamEpicMemberArkhamEpicEventId P.==. eid, ArkhamEpicMemberUserId P.==. userId]
            []
      let resolvedRole =
            if isOrganizer
              then Just Organizer
              else arkhamEpicMemberRole . entityVal <$> mRole
      digests <- traverse (mkGroupDigest userId) groupRows
      pure
        EventDetails
          { id = eid
          , name = arkhamEpicEventName event
          , organizerUserId = arkhamEpicEventOrganizerUserId event
          , role = resolvedRole
          , sharedState = arkhamEpicEventSharedState event
          , totalInvestigators = arkhamEpicEventTotalInvestigators event
          , groups = digests
          }

mkGroupDigest :: UserId -> Entity ArkhamEpicGroup -> Handler GroupDigest
mkGroupDigest userId (Entity _ grp) = case arkhamEpicGroupArkhamGameId grp of
  Nothing ->
    pure
      GroupDigest
        { ordinal = ordx
        , name = nm
        , gameId = Nothing
        , gameState = Nothing
        , investigatorCount = 0
        , seatCount = seats
        , youAreSeated = False
        , players = []
        }
  Just gid -> do
    mGame <- runDB $ P.get gid
    seated <- runDB $ P.exists [ArkhamPlayerArkhamGameId P.==. gid, ArkhamPlayerUserId P.==. userId]
    playerRows <- runDB $ select do
      (p :& u) <-
        from
          $ table @ArkhamPlayer
            `innerJoin` table @User
          `on` (\(p :& u) -> p.userId ==. u.id)
      where_ $ p.arkhamGameId ==. val gid
      pure (p.id, u.username)
    let
      investigators :: [Investigator]
      investigators = maybe [] (toList . gameInvestigators . arkhamGameCurrentData) mGame
      invByPlayer :: Map PlayerId InvestigatorId
      invByPlayer = Map.fromList [(attr investigatorPlayerId i, i.id) | i <- investigators]
      players =
        [ GroupPlayerInfo {username = un, investigatorId = Map.lookup (PlayerId (coerce pid)) invByPlayer}
        | (Value pid, Value un) <- playerRows
        ]
    pure
      GroupDigest
        { ordinal = ordx
        , name = nm
        , gameId = Just gid
        , gameState = gameGameState . arkhamGameCurrentData <$> mGame
        , investigatorCount = length players
        , seatCount = seats
        , youAreSeated = seated
        , players = players
        }
 where
  ordx = arkhamEpicGroupOrdinal grp
  nm = arkhamEpicGroupName grp
  seats = arkhamEpicGroupSeatCount grp

{- | Create one group's game as an OPEN multiplayer lobby: a WithFriends game with
@playerCount@ seats and no players yet. It stays in 'IsPending' until players
join asynchronously via @PUT /games/:id/join@, then activates and runs setup
once its seats fill — exactly the normal multiplayer flow, one lobby per group.
The organizer is NOT auto-seated (they may join a group like anyone else).
-}
createGroupGame
  :: Text -> ScenarioId -> Difficulty -> Bool -> Int -> Handler ArkhamGameId
createGroupGame gameName scenarioId difficulty includeTarotReadings playerCount = do
  newGameSeed <- liftIO getRandom
  now <- liftIO getCurrentTime
  let
    seats = max 1 playerCount
    -- Flag the group's scenario as Epic Multiplayer so it picks its epic setup
    -- branch at Setup time (the join path runs setup with no event context).
    game =
      setInitialScenarioMeta "epicMultiplayer" True
        $ newScenario scenarioId newGameSeed seats difficulty includeTarotReadings
    ag = ArkhamGame gameName game 0 WithFriends now now
  runDB do
    gameId <- P.insert ag
    P.insert_ $ ArkhamStep gameId (Choice mempty []) 0 (ActionDiff [])
    pure gameId

-- | Initial shared counters for an event, by scenario. Frozen at event start
-- (scales by the total investigator count across all groups).
epicScenarioSeeds :: ScenarioId -> Int -> [(SharedKey, Int)]
epicScenarioSeeds scenarioId total
  | scenarioId == "85001" =
      -- The Blob That Ate Everything: countermeasures = ceil(total/2); Subject
      -- 8L-08 (epic, card 85037) global health = 15 x total.
      [ (Countermeasures, (total + 1) `div` 2)
      , (SharedEnemyHealth (CardCode "85037"), 15 * total)
      ]
  | otherwise = []

-- | The per-event websocket: a read-only feed of shared-state updates.
eventStream :: ArkhamEpicEventId -> WebSocketsT Handler ()
eventStream eid = do
  room <- lift $ getEventRoom eid
  streamRoom (eventChannel eid) room do
    roomsVar <- lift $ getsYesod appEventRooms
    liftIO $ modifyMVar_ roomsVar $ pure . Map.delete eid
    lift $ removeChannel (eventChannel eid)
