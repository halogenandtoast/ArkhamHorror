{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards -Wno-unused-imports -Wno-unused-matches -Wno-missing-signatures -Wno-orphans #-}

module Arkham.Investigator.Runner.Movement where

import Arkham.Ability as X hiding (PaidCost)
import Arkham.ChaosToken as X
import Arkham.ClassSymbol as X
import Arkham.Classes as X
import Arkham.ForMovement
import Arkham.Helpers.Investigator as X
import Arkham.Helpers.Message as X hiding (
  InvestigatorDamage,
  InvestigatorDefeated,
  InvestigatorResigned,
 )
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Investigator.Types as X
import Arkham.Name as X
import Arkham.Source as X
import Arkham.Stats as X
import Arkham.Target as X
import Arkham.Trait as X hiding (Cosmos, Cultist, ElderThing, Haunted)
import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap

import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Actions (actionsToList)
import Arkham.Asset.Types (Field (..))
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Card.Settings
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Constants
import Arkham.Cost qualified as Cost
import Arkham.Customization
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Discard
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Types (Field (..))
import Arkham.Fight.Types
import {-# SOURCE #-} Arkham.Game (asIfTurn, withoutCanModifiers)
import Arkham.Game.Settings (settingsStrictAsIfAt)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Ability (
  getAbilityLimit,
  getCanAffordUseWith,
  getCanPerformAbility,
  isForcedAbility,
 )
import Arkham.Helpers.Action (
  additionalActionCovers,
  canDo_,
  getActions,
  getActionsWith,
  getAdditionalActions,
  getCanAfford,
 )
import Arkham.Helpers.Card (
  cardIsFast',
  drawThisCardFrom,
  extendedCardMatch,
  getModifiedCardCost,
  passesLimits,
 )
import Arkham.Helpers.Cost (getCanAffordCost, getSpendableResources, hasSkillTestCost)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Deck qualified as Deck
import Arkham.Helpers.Discover
import Arkham.Helpers.Enemy (canEnterLocation, getModifiedKeywords)
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Location (
  getCanMoveTo,
  getCanMoveToMatchingLocations,
  isDiscoveringLastClue,
  withLocationOf,
 )
import Arkham.Helpers.Log (hasCampaignOption)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable (getIsPlayable, getIsPlayableWithResources, getPlayableCards)
import Arkham.Helpers.Ref (sourceToCard)
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Slot (
  canPutIntoSlot,
  emptySlot,
  removeIfMatches,
  removeIfMatchesOnce,
  slotItems,
 )
import Arkham.Helpers.Source (sourceMatches, sourceTraits)
import Arkham.Helpers.Window (
  batchedTimings,
  checkWhen,
  checkWindows,
  frame,
  pushBatch,
  pushBatched,
  timings,
  windowMatches,
  wouldDo,
 )
import Arkham.Helpers.Window qualified as Helpers
import Arkham.History
import Arkham.I18n (countVar, ikey', withI18n)
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Runner.Damage
import Arkham.Investigator.Types qualified as Attrs
import Arkham.Key
import Arkham.Keyword (Keyword (Starting))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  AssetMatcher (..),
  CardMatcher (..),
  EnemyMatcher (..),
  EventMatcher (..),
  ExtendedCardMatcher (..),
  ForPlay (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  ScenarioMatcher (..),
  SourceMatcher (..),
  WindowMatcher (AnyWindow),
  assetControlledBy,
  assetIs,
  at_,
  basic,
  cardIs,
  colocatedWith,
  enemyEngagedWith,
  inHandOf,
  locationWithInvestigator,
  oneOf,
  orConnected,
  pattern AnyInPlayEnemy,
  pattern AssetWithAnyClues,
 )
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (obtainCard, takeControlOfAsset)
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Message.Lifted.Choose qualified as Choose
import Arkham.Message.Lifted.Move (moveTo, moveToEdit)
import Arkham.Modifier
import Arkham.Modifier qualified as Modifier
import Arkham.Movement
import Arkham.Phase
import Arkham.Placement
import Arkham.Plural
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Search hiding (drawnCardsL, foundCardsL)
import Arkham.Search qualified as Search
import Arkham.Skill.Types (Field (..))
import Arkham.SkillTest
import Arkham.Slot
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Window (Window (..), defaultWindows, mkAfter, mkWhen, mkWindow, primaryWindowTarget)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, non, over, sumOf, _Just)
import Control.Monad.State.Strict (evalStateT, get, modify)
import Data.Data.Lens (biplate)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set
import Data.UUID (nil)

handleMoveAction a@InvestigatorAttrs {..} iid lid cost = do
  beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #move)]
  afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #move)]
  pushAll
    [ BeginAction
    , beforeWindowMsg
    , TakeActions iid [#move] cost
    , MoveAction iid lid cost False
    , afterWindowMsg
    , FinishAction
    , TakenActions iid [#move]
    ]
  movement <- move iid iid lid
  pure $ a & movementL ?~ movement

handleMoveActionV2 a@InvestigatorAttrs {..} iid lid = do
  from <- fromMaybe (LocationId nil) <$> field InvestigatorLocation iid
  afterWindowMsg <- Helpers.checkWindows [mkAfter $ Window.MoveAction iid from lid]
  mods <- getModifiers iid
  let canMove =
        none
          (`elem` mods)
          (CannotMove : [CancelMovement movement.id | movement <- maybeToList investigatorMovement])
  if canMove
    then pushAll . resolve . Move =<< move a iid lid
    else push $ MoveIgnored iid
  push afterWindowMsg
  pure a

handleMove a@InvestigatorAttrs {..} movement = do
  scenarioEffect <- sourceMatches movement.source SourceIsScenarioCardEffect
  canMove <-
    withoutModifiers a $ CannotMove
      : CancelMovement movement.id
      : [CannotMoveExceptByScenarioCardEffects | not scenarioEffect]
  when (canMove || not movement.cancelable) do
    case moveDestination movement of
      ToLocationMatching matcher -> do
        lids <- getCanMoveToMatchingLocations investigatorId (moveSource movement) matcher
        -- The only match can be the location you are already at (Salem Gaol's
        -- haunted ability while you stand in Keziah's Room), which this query
        -- excludes. A move with nowhere to go is a no-op, not a crash. See #5483.
        unless (null lids) do
          player <- getPlayer investigatorId
          push
            $ chooseOrRunOne player
            $ [targetLabel lid [Move $ movement {moveDestination = ToLocation lid}] | lid <- lids]
      ToLocation destinationLocationId -> do
        batchId <- getRandom

        let
          source = moveSource movement
          iid = investigatorId
        mFromLocation <- field InvestigatorLocation iid

        case moveMeans movement of
          TowardsN n -> do
            player <- getPlayer iid
            let loc = fromJustNote "must have a starting location for OneAtATime" mFromLocation
            matchingClosestLocationIds <- select $ ClosestPathLocation loc destinationLocationId
            if destinationLocationId `elem` matchingClosestLocationIds
              then
                push $ chooseOne player [targetLabel destinationLocationId [Move $ movement {moveMeans = Direct}]]
              else do
                Choose.chooseTargetM iid matchingClosestLocationIds \lid -> do
                  pushAll
                    $ Move (movement {moveDestination = ToLocation lid, moveMeans = Direct})
                    : [Move $ movement {moveMeans = TowardsN (n - 1)} | n > 1]
          Towards -> do
            player <- getPlayer iid
            let loc = fromJustNote "must have a starting location for OneAtATime" mFromLocation
            matchingClosestLocationIds <- select $ ClosestPathLocation loc destinationLocationId
            if destinationLocationId `elem` matchingClosestLocationIds
              then
                push $ chooseOne player [targetLabel destinationLocationId [Move $ movement {moveMeans = Direct}]]
              else do
                push
                  $ chooseOne
                    player
                    [ targetLabel
                        lid
                        [Move $ movement {moveDestination = ToLocation lid, moveMeans = Direct}]
                    | lid <- matchingClosestLocationIds
                    ]
          OneAtATime -> do
            player <- getPlayer iid
            let loc = fromJustNote "must have a starting location for OneAtATime" mFromLocation
            matchingClosestLocationIds <- select $ ClosestPathLocation loc destinationLocationId
            if destinationLocationId `elem` matchingClosestLocationIds
              then
                push $ chooseOne player [targetLabel destinationLocationId [Move $ movement {moveMeans = Direct}]]
              else do
                push
                  $ chooseOne
                    player
                    [ targetLabel
                        lid
                        [Move $ movement {moveDestination = ToLocation lid, moveMeans = Direct}, Move movement]
                    | lid <- matchingClosestLocationIds
                    ]
          Direct -> do
            imods <- getModifiers investigatorId
            leaveCosts <- case mFromLocation of
              Nothing -> pure mempty
              Just lid ->
                if movePayAdditionalCosts movement
                  then do
                    mods' <- getModifiers lid
                    pure $ mconcat [c | AdditionalCostToLeave c <- mods']
                  else pure mempty

            enterCosts <- do
              if movePayAdditionalCosts movement
                then do
                  revealed' <- field LocationRevealed destinationLocationId
                  baseEnter <- mwhen (not revealed') <$> field LocationCostToEnterUnrevealed destinationLocationId

                  mods' <- getModifiers destinationLocationId
                  pcosts <-
                    filterM ((destinationLocationId <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
                  pure $ concatMap snd pcosts
                    <> mconcat [c | AdditionalCostToEnter c <- mods']
                    <> moveAdditionalEnterCosts movement
                    <> baseEnter
                else pure mempty

            let
              (whenMoves, atIfMoves, afterMoves) = timings (Window.Moves iid source mFromLocation destinationLocationId movement.id)
              (mWhenLeaving, mAtIfLeaving, mAfterLeaving) = case mFromLocation of
                Just from ->
                  batchedTimings batchId (Window.Leaving iid from) & \case
                    (whens, atIfs, afters) -> (Just whens, Just atIfs, Just afters)
                Nothing -> (Nothing, Nothing, Nothing)
              mWouldMove = case mFromLocation of
                Just from ->
                  batchedTimings batchId (Window.WouldMove iid source from destinationLocationId) & \case
                    (whens, _, _) -> Just whens
                Nothing -> Nothing
              (whenEntering, atIfEntering, _) = batchedTimings batchId (Window.Entering iid destinationLocationId)

            -- Windows we need to check as understood:
            -- when {leaving} -> atIf {leaving} -> after {leaving} -> before {entering} -> atIf {entering} / when {move} -> atIf {move} -> Reveal Location -> after but before enemy engagement {entering} -> Check Enemy Engagement -> after {entering, move}
            -- A plain "after you enter a location" reaction (e.g. On Their Heels)
            -- resolves AFTER enemies engage. This is established by Track Shoes,
            -- which has to spell out "after you move, but before enemies engage
            -- you" precisely because the default after-enter timing is post
            -- engagement (FAQ v2.5 Q&A #034). So the after-entering window fires
            -- after CheckEnemyEngagement (e.g. Zoey's Cross). See #4813.
            -- move but before enemy engagement is handled in MoveTo

            mRunWouldMove <- case mWouldMove of
              Just wouldMove -> Just <$> checkWindows [wouldMove]
              Nothing -> pure Nothing
            mRunWhenLeaving <- case mWhenLeaving of
              Just whenLeaving -> Just <$> checkWindows [whenLeaving]
              Nothing -> pure Nothing
            mRunAtIfLeaving <- case mAtIfLeaving of
              Just atIfLeaving -> Just <$> checkWindows [atIfLeaving]
              Nothing -> pure Nothing
            mRunAfterLeaving <- case mAfterLeaving of
              Just afterLeaving -> Just <$> checkWindows [afterLeaving]
              Nothing -> pure Nothing
            runWhenMoves <- checkWindows [whenMoves]
            runAtIfMoves <- checkWindows [atIfMoves]
            runWhenEntering <- checkWindows [whenEntering]
            runAtIfEntering <- checkWindows [atIfEntering]
            runAfterMoves <- checkWindows [afterMoves]

            let
              postEnterMsgs =
                [ runWhenMoves
                , runAtIfMoves
                , ResolveMovement iid
                , runAfterMoves
                , ResolvedMovement iid movement.id
                ]
                  <> maybeToList mRunAfterLeaving
              innerMsgs =
                [MoveFrom source iid fromLocationId | fromLocationId <- maybeToList mFromLocation]
                  <> [runWhenEntering, runAtIfEntering, PayAdditionalCost iid batchId enterCosts]
                  <> if hasSkillTestCost enterCosts
                    then [MoveWithSkillTest (WhenCanMove iid postEnterMsgs)]
                    else postEnterMsgs
              whenCanMoveMsg = WhenCanMove iid innerMsgs
              wrapLeave = if hasSkillTestCost leaveCosts then MoveWithSkillTest else id
            pushBatched batchId
              $ maybeToList mRunWouldMove
              <> maybeToList mRunWhenLeaving
              <> maybeToList mRunAtIfLeaving
              <> [PayAdditionalCost iid batchId leaveCosts]
            push (wrapLeave whenCanMoveMsg)
          Place -> do
            -- like Direct, but no moves windows and no costs

            let
              (whenEntering, atIfEntering, _) = batchedTimings batchId (Window.Entering iid destinationLocationId)
              (mWhenLeaving, mAtIfLeaving, mAfterLeaving) = case mFromLocation of
                Just from ->
                  batchedTimings batchId (Window.Leaving iid from) & \case
                    (whens, atIfs, afters) -> (Just whens, Just atIfs, Just afters)
                Nothing -> (Nothing, Nothing, Nothing)
            mRunWhenLeaving <- for mWhenLeaving \whenLeaving -> checkWindows [whenLeaving]
            mRunAtIfLeaving <- for mAtIfLeaving \atIfLeaving -> checkWindows [atIfLeaving]
            mRunAfterLeaving <- for mAfterLeaving \afterLeaving -> checkWindows [afterLeaving]
            runWhenEntering <- checkWindows [whenEntering]
            runAtIfEntering <- checkWindows [atIfEntering]

            pushBatched batchId
              $ maybeToList mRunWhenLeaving
              <> maybeToList mRunAtIfLeaving
              <> [ runWhenEntering
                 , runAtIfEntering
                 , MoveTo movement
                 ]
              <> maybeToList mRunAfterLeaving
  pure $ a & movementL ?~ movement

-- The movement slot is single-valued. A nested move that runs while this one is
-- parked on an additional-cost skill test (Another Dimension's forced move when
-- Arcane Barrier's leave cost discards the location) consumes the slot and
-- clears it in handleDoResolveMovement. Resuming with an empty slot used to push
-- the whole batch anyway, firing MoveFrom/Entering/Moves windows -- and so
-- "after you are moved" reactions -- for a move that can no longer happen. See
-- #5412.
handleWhenCanMove a@InvestigatorAttrs {..} iid msgs = do
  mods <- getModifiers iid
  for_ investigatorMovement \movement -> do
    let canMove = none (`elem` mods) [CannotMove, CancelMovement movement.id]
    when (canMove || not movement.cancelable) $ pushAll msgs
  pure a

handleMoveAllTo a@InvestigatorAttrs {..} source lid = do
  moveToEdit source investigatorId lid \m ->
    m
      { moveMeans = Place
      , movePayAdditionalCosts = False
      , moveCancelable = False
      }
  pure a

handleMoveToward a@InvestigatorAttrs {..} target locationMatcher = do
  mods <- getModifiers investigatorId
  unless (CannotMove `elem` mods) do
    withLocationOf investigatorId \loc -> do
      mlid <- selectOne locationMatcher
      for_ mlid \lid -> when (lid /= loc) do
        closestLocationIds <- select $ ClosestPathLocation loc lid
        when (notNull closestLocationIds) do
          Choose.chooseTargetM investigatorId closestLocationIds $ moveTo GameSource investigatorId
  pure a

handleMoveUntil a@InvestigatorAttrs {..} lid target = do
  mods <- getModifiers investigatorId
  unless (CannotMove `elem` mods) do
    withLocationOf investigatorId \loc ->
      when (lid /= loc) do
        closestLocationIds <- select $ ClosestPathLocation loc lid
        when (notNull closestLocationIds) do
          Choose.chooseTargetM investigatorId closestLocationIds \nextLid -> do
            moveTo GameSource investigatorId nextLid
            push $ MoveUntil lid target
  pure a

handleMoveTo a@InvestigatorAttrs {..} movement = do
  pushAll [ResolveMovement investigatorId, ResolvedMovement investigatorId movement.id]
  pure $ a & movementL ?~ movement

handleSetMovement a@InvestigatorAttrs {..} iid movement = do
  pure $ a & movementL ?~ movement

handleResolvedMovement a@InvestigatorAttrs {..} iid movementId = do
  pure
    $ a
    & ( usedAbilitiesL
          %~ filter (\ab -> ab.limitType /= Just PerMove && ab.limitType /= Just (PerMovement movementId))
      )

handleResolveMovement a@InvestigatorAttrs {..} iid msg = do
  mods <- getModifiers iid
  let
    isForcedMove = maybe False (.forced) investigatorMovement
    canMove =
      none
        (`elem` mods)
        (CannotMove : [CancelMovement movement.id | movement <- maybeToList investigatorMovement])
  when (canMove || isForcedMove) $ push $ Do msg
  pure a

handleDoResolveMovement a@InvestigatorAttrs {..} iid = do
  case investigatorMovement of
    Nothing -> pure a
    Just movement -> case moveDestination movement of
      ToLocationMatching matcher -> do
        lids <- select matcher
        player <- getPlayer investigatorId
        unless (null lids) do
          push
            $ chooseOrRunOne
              player
              [targetLabel lid [MoveTo $ movement {moveDestination = ToLocation lid}] | lid <- lids]
        pure a
      ToLocation lid -> do
        -- Engaged, non-massive enemies that can enter follow the investigator
        -- into lid; others disengage. We push EnemyEnteredFollowing for each
        -- follower inside a Simultaneously block alongside the investigator's
        -- own entry messages so their CheckWindows merge at one sync point.
        -- In particular this puts EnterLocation's RevealLocation push in the
        -- pre-window pool so the location is revealed (and clues placed)
        -- before the EnemyEnters After window fires — reactions like Glassing
        -- then see the post-reveal state.
        -- Swarm cards are reported as engaged alongside their host
        -- (`enemyEngagedInvestigators` recurses through `AsSwarm`), and both
        -- `EnemyEnteredFollowing` and `DisengageEnemy` redirect a swarm card
        -- back to its host. Deciding per card therefore lets a swarm card's
        -- follow message drag a host that was just left behind (#5313), so
        -- collapse to the host and push one message per group. Swarming X:
        -- the host and its swarm cards "move, engage, and exhaust as a single
        -- entity", so any member's CannotMove holds the whole group.
        engagedEnemies <- select $ enemyEngagedWith iid
        movementUnits <- fmap nub $ for engagedEnemies \eid ->
          field Field.EnemyPlacement eid <&> \case
            AsSwarm host _ -> host
            _ -> eid

        (followers, disengagers) <-
          partitionM
            ( \eid -> do
                keywords <- getModifiedKeywords eid
                canEnter <- canEnterLocation eid lid
                swarm <- select $ SwarmOf eid
                canMove <- flip allM (eid : swarm) \swarmMember -> do
                  mods <- getModifiers swarmMember
                  pure $ CannotMove `notElem` mods && CannotBeMoved `notElem` mods
                pure (#massive `notElem` keywords && canEnter && canMove)
            )
            movementUnits

        pushAll
          -- Disengage the left-behind enemies BEFORE the investigator enters the
          -- new location. DisengageEnemy re-homes an enemy from a threat area to
          -- the engaged investigator's *current* location, so it must run while
          -- the investigator is still at the location being left — otherwise a
          -- non-following enemy (e.g. a Geist held by Ghost Light's CannotMove)
          -- would be dropped at the destination and immediately re-engaged.
          $ [DisengageEnemy iid eid | eid <- disengagers]
          <> [WhenWillEnterLocation iid lid]
          <> [ Simultaneously
                 $ Run [Do (WhenWillEnterLocation iid lid), EnterLocation iid lid]
                 : [EnemyEnteredFollowing iid eid lid | eid <- followers]
             ]
          <> [After (MoveTo movement)]

        -- Safeguard-style "move with" only covers a move "from your location to
        -- a connecting location". A card effect that sends the mover to an
        -- arbitrary location (Pendant of the Queen) is not something a follower
        -- may tag along with, so gate on the destination actually being
        -- accessible from where the follower is standing (#5407). `iid` has not
        -- been re-placed yet here, so every follower selected still shares the
        -- origin location with them.
        when (movement.means /= Place) do
          moveWith <-
            select (InvestigatorWithModifier (CanMoveWith $ InvestigatorWithId iid) <> colocatedWith iid)
          for_ moveWith \iid' -> do
            connected <- lid <=~> AccessibleFrom ForMovement (locationWithInvestigator iid')
            when connected $ push $ ForInvestigator iid' $ ForTarget (LocationTarget lid) (MoveTo movement)

        afterMoveButBeforeEnemyEngagement <-
          Helpers.checkWindows [mkAfter (Window.MovedButBeforeEnemyEngagement iid lid)]
        -- Snapshot enemy presence at entry (before engagement, before anything can
        -- defeat them) so "after you enter a location with 1+ enemies" triggers
        -- survive an enemy being defeated during engagement. The snapshot window is
        -- offered alongside the regular after-Entering window. See #4813.
        enteredWithEnemy <- selectAny (EnemyAt (LocationWithId lid))
        afterEntering <-
          Helpers.checkWindows
            $ mkAfter (Window.Entering iid lid)
            : [mkAfter (Window.EnteringLocationWithEnemy iid lid) | enteredWithEnemy]
        -- An enemy left standing where we were -- disengaged just above because it
        -- could not follow, or disengaged by the effect that moved us (Gate Box,
        -- Cat Burglar, On Wings of Darkness) -- is unengaged at a location that
        -- still holds investigators, and nothing rechecked until `After (EndTurn _)`
        -- (#5500). `iid` has not been re-placed yet, so this is the origin.
        stayingBehind <- select $ colocatedWith iid <> NotInvestigator (InvestigatorWithId iid)
        pushAll $ moveAfter movement
          <> [afterMoveButBeforeEnemyEngagement | movement.means /= Place]
          <> [CheckEnemyEngagement iid | not movement.skipEngagement]
          <> [afterEntering]
          <> map CheckEnemyEngagement stayingBehind
        pure $ a & movementL .~ Nothing

handleDoWhenWillEnterLocation a@InvestigatorAttrs {..} iid lid = do
  let prevLoc = case investigatorPlacement of
        AtLocation l -> Just l
        _ -> Nothing
  pure $ a & placementL .~ AtLocation lid & previousLocationL .~ prevLoc

handleSwapPlaces a@InvestigatorAttrs {..} aTarget newLocation = do
  push $ CheckEnemyEngagement a.id
  pure $ a & placementL .~ AtLocation newLocation

handleSwapPlacesV2 a@InvestigatorAttrs {..} newLocation bTarget = do
  push $ CheckEnemyEngagement a.id
  pure $ a & placementL .~ AtLocation newLocation

handleRemovedLocation a@InvestigatorAttrs {..} lid = do
  -- needs to look at the "real" location not as if
  pure $ a & placementL .~ Unplaced

handleDoPlaceInvestigator a@InvestigatorAttrs {..} iid placement = do
  when (placement == Unplaced) do
    enemies <- select $ enemyEngagedWith iid
    pushAll $ case investigatorLocation a of
      Just lid -> [PlaceEnemy enemy (AtLocation lid) | enemy <- enemies]
      Nothing -> [toDiscard GameSource (toTarget enemy) | enemy <- enemies]

  pure $ a & placementL .~ placement

investigatorLocation :: InvestigatorAttrs -> Maybe LocationId
investigatorLocation a = preview _AtLocation a.placement
