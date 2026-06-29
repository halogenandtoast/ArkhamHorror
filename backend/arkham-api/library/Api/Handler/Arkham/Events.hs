{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
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
  postApiV1ArkhamEventTimeUpR,
  postApiV1ArkhamEventReadyR,
  postApiV1ArkhamEventResolveAdvanceR,
) where

import Api.Arkham.Epic (applyEpicDeltasLocked, modifySharedStateLocked)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Games.Shared (broadcastSharedToEvent, deleteEventRoom, deleteRoom, getEventGroupContributions, getEventGroupGameIds, propagateShared, runMessagesInGroupWhen, settleOrganizerAdvance, streamRoom)
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (agendaSequence)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Difficulty (Difficulty)
import Arkham.Entities (entitiesAgendas)
import Arkham.Epic.Types
import Arkham.Classes.Entity (attr)
import Arkham.Game (Game, gameEntities, gameGameState, newScenario, setInitialScenarioMeta)
import Arkham.Message (Message (AdvanceToAgenda))
import Arkham.Source (Source (GameSource))
import Arkham.Game.State (GameState)
import Arkham.Game.Utils (gameInvestigators)
import Arkham.Id (InvestigatorId, PlayerId (..), ScenarioId)
import Arkham.Investigator.Types (Investigator, investigatorPlayerId)
import Control.Concurrent.MVar (modifyMVar_)
import Control.Monad.Random.Class (getRandom)
import Data.Bits (shiftL, (.|.))
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Esqueleto.Experimental hiding (isNothing, update, (=.))
import Database.Persist qualified as P
import Entity.Arkham.Step (ActionDiff (..), ArkhamStep (..), Choice (..))
import Import hiding (on, (==.))
import UnliftIO.Exception (catch)
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
  , timeLimitMinutes :: Maybe Int
  -- ^ optional Epic time limit (default 180); when elapsed, still-playing groups
  -- are forced to agenda 3b.
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

-- | One group's organizer-allocated spend toward a stage advance.
data AllocationEntry = AllocationEntry
  { ordinal :: Int
  , spend :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Body of @POST events/{id}/resolve-advance@: the organizer's per-group spend
-- allocation for a stage awaiting resolution.
data ResolveAdvancePost = ResolveAdvancePost
  { stage :: Int
  , allocation :: [AllocationEntry]
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
  , createdAt :: UTCTime
  -- ^ event start; the time-limit countdown runs from here.
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
  -- A single random per-event seed; groups derive the shared Act 3b story-card
  -- pick deterministically from it (so all groups agree with no cross-group race).
  storySeed <- (`mod` 1000000) <$> liftIO getRandom

  let
    totalInvestigators = sum (map (.playerCount) groups)
    seeded =
      setSharedCounter TimeLimitMinutes (fromMaybe 180 timeLimitMinutes)
        $ setSharedCounter BlobStorySeed storySeed
        $ foldr
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
      -- Update every client's shared store and sync all groups' boards.
      propagateShared eid Nothing newState

{- | The Epic time limit has elapsed: force every still-playing group to agenda
3b ("face the consequences"). The frontend posts here when its (createdAt +
TimeLimitMinutes) countdown hits 0; because that deadline is identical for every
client, several may fire near-simultaneously, so this must be idempotent.

For each group game we drive the Blob's stage-3 agenda (theAnomalyConsumes) to
its B side via 'AdvanceToAgenda' — the deck-id (1) form, NOT the stage; the agenda
card def carries the stage. Advancing to side B replaces the current agenda with
stage 3, flips it (AgendaAdvancedWithOther), and the card's
@AdvanceAgenda (isSide B)@ handler pushes R1, the lose-by-time resolution. The
flip parks a lead-player confirmation; 'runMessagesInGroupWhen' persists that
continuation so the resolution completes when confirmed.

Idempotency: skip any group that is not 'IsActive' or already at/past agenda
stage 3. The stage check runs INSIDE each group's FOR UPDATE lock (the @p@
predicate of 'runMessagesInGroupWhen'), so concurrent expiry calls serialize and
never double-advance — once one call advances a group to stage 3, every other
call sees stage 3 and skips it. Per-group failures are logged and isolated so one
bad group can't block forcing the rest.
-}
postApiV1ArkhamEventTimeUpR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventTimeUpR eid = do
  userId <- getRequestUserId
  void $ requireEventMember userId eid
  gameIds <- getEventGroupGameIds eid
  for_ gameIds \gid ->
    runMessagesInGroupWhen
      (not . agendaAtOrPastStage 3)
      [AdvanceToAgenda 1 Agendas.theAnomalyConsumes Agenda.B GameSource]
      gid
      `catch` \(e :: SomeException) ->
        $(logWarn) $ "Epic time-up advance failed for " <> tshow gid <> ": " <> tshow e

{- | Start-of-game barrier: mark the caller's group ready (idempotent, by group
ordinal bit). When EVERY group is ready, the time-limit timer starts (records the
epoch). The frontend calls this once its group reaches the first investigation
phase, and gates play until 'TimerStartedAt' is set. No-op for a caller without a
seated group (e.g. an organizer who isn't playing).
-}
postApiV1ArkhamEventReadyR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventReadyR eid = do
  userId <- getRequestUserId
  void $ requireEventMember userId eid
  mOrdinal <- runDB do
    mMember <-
      P.selectFirst
        [ ArkhamEpicMemberArkhamEpicEventId P.==. eid
        , ArkhamEpicMemberUserId P.==. userId
        , ArkhamEpicMemberRole P.==. GroupPlayer
        ]
        []
    pure $ mMember >>= (arkhamEpicMemberGroupOrdinal . entityVal)
  for_ mOrdinal \ordinal -> do
    numGroups <- runDB $ P.count [ArkhamEpicGroupArkhamEpicEventId P.==. eid]
    now <- liftIO getCurrentTime
    let
      nowEpoch = floor (utcTimeToPOSIXSeconds now) :: Int
      fullMask = (1 `shiftL` numGroups) - 1
    newState <- runDB $ modifySharedStateLocked eid \s ->
      let
        mask' = sharedCounter GroupsReadyMask s .|. (1 `shiftL` ordinal)
        s' = setSharedCounter GroupsReadyMask mask' s
      in
        if mask' == fullMask && sharedCounter TimerStartedAt s == 0
          then setSharedCounter TimerStartedAt nowEpoch s'
          else s'
    broadcastSharedToEvent eid newState

{- | Organizer-mediated excess-clue distribution on a shared act advance. The
coordinator has gated the stage with @AwaitingOrganizer stage == 1@; the organizer
allocates how many of each group's contributed clues are spent toward the
threshold. 200 with empty body — the result is pushed over the websocket.

Validation is server-side from the current shared state: every @spend@ in
@[0, that group's contribution]@ and @sum spend == 2 * sharedTotalInvestigators@.
The authoritative consume (write per-group 'ActSpend', reset the pool, bump the
generation, clear the gate) + the replica mirror + the global undo floor + the
overlay-lifting broadcast all happen in 'settleOrganizerAdvance', which is atomic
and idempotent against a double-submit. NO gameplay message is injected into any
group; the parked act reads its own 'ActSpend' from its mirrored replica.
-}
postApiV1ArkhamEventResolveAdvanceR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventResolveAdvanceR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  ResolveAdvancePost {..} <- requireCheckJsonBody
  mEvent <- runDB $ P.get eid
  case mEvent of
    Nothing -> notFound
    Just event -> do
      let
        shared0 = arkhamEpicEventSharedState event
        threshold = 2 * sharedTotalInvestigators shared0
      when (sharedCounter (AwaitingOrganizer stage) shared0 /= 1)
        $ invalidArgs ["No advance awaiting organizer for this stage"]
      contributions <- getEventGroupContributions eid stage
      let
        contribMap = Map.fromList contributions
        -- Aggregate by ordinal so duplicate entries can't defeat a per-group cap.
        spendByOrdinal = Map.fromListWith (+) [(entry.ordinal, entry.spend) | entry <- allocation]
        totalSpend = sum (Map.elems spendByOrdinal)
        invalidGroup (ordinal, spend) = spend < 0 || spend > Map.findWithDefault 0 ordinal contribMap
      when (totalSpend /= threshold)
        $ invalidArgs ["Allocation must spend exactly " <> tshow threshold <> " clues"]
      when (any invalidGroup (Map.toList spendByOrdinal))
        $ invalidArgs ["A group's spend is negative or exceeds its contribution"]
      settleOrganizerAdvance eid stage spendByOrdinal

-- | Whether any agenda currently in play in the group's game is at or past
-- @stage@. Used as the in-lock idempotency guard for the time-up forcing: a group
-- already at agenda stage 3 (forced previously, or advanced there in normal play)
-- is left untouched.
agendaAtOrPastStage :: Int -> Game -> Bool
agendaAtOrPastStage stage game =
  any
    (\ag -> Agenda.agendaSequenceStep (attr agendaSequence ag) >= stage)
    (toList (entitiesAgendas (gameEntities game)))

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
          , createdAt = arkhamEpicEventCreatedAt event
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
      -- 8L-08 (epic, card 85037) global health = 15 x total. Act 1/3 shared clue
      -- progress is seeded at 0 so the keys exist in shared state from the start
      -- (lets the UI display "clues X / 2*total" for the active clue-threshold act
      -- before any clues are contributed).
      [ (Countermeasures, (total + 1) `div` 2)
      , (SharedEnemyHealth (CardCode "85037"), 15 * total)
      , (SharedActProgress 1, 0)
      , (SharedActProgress 3, 0)
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
