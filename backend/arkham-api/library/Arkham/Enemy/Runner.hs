{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Enemy.Runner (module Arkham.Enemy.Runner, module X) where

import Arkham.Ability as X
import Arkham.Calculation as X
import Arkham.Enemy.Helpers as X hiding (EnemyEvade, EnemyFight)
import Arkham.Enemy.Types as X
import Arkham.GameValue as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Enemy as X
import Arkham.Helpers.Message as X hiding (
  EnemyAttacks,
  EnemyDamage,
  EnemyDefeated,
  EnemyEvaded,
  InvestigatorDefeated,
  PaidCost,
  PhaseStep,
 )
import Arkham.Helpers.SkillTest as X
import Arkham.Id as X (AsId (..))
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Spawn as X
import Arkham.Target as X

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.DefeatedBy
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator
import Arkham.Helpers.Placement
import Arkham.History
import Arkham.Id
import Arkham.Keyword (_Swarming)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher (
  AssetMatcher (..),
  EnemyMatcher (..),
  InvestigatorMatcher (..),
  LocationFilter (..),
  LocationMatcher (..),
  MovesVia (..),
  PreyMatcher (..),
  be,
  investigatorAt,
  investigatorEngagedWith,
  locationWithEnemy,
  locationWithInvestigator,
  mapOneOf,
  oneOf,
  preyWith,
  replaceYouMatcher,
  pattern InvestigatorCanDisengage,
  pattern MassiveEnemy,
 )
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType ()
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Control.Lens (non, _Just)
import Data.Function (on)
import Data.List (nubBy)
import Data.List qualified as List
import Data.List.Extra (firstJust)
import Data.Monoid (First (..))

{- | Handle when enemy no longer exists
When an enemy is defeated we need to remove related messages from choices
and if not more choices exist, remove the message entirely
-}
filterOutEnemyMessages :: EnemyId -> Message -> Maybe Message
filterOutEnemyMessages eid ask'@(Ask pid q) = case q of
  QuestionLabel {} -> Just ask'
  Read {} -> Just ask'
  DropDown {} -> Just ask'
  PickSupplies {} -> Just ask'
  ChooseOne msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseOne x)
  PlayerWindowChooseOne msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ PlayerWindowChooseOne x)
  ChooseOneFromEach groups -> case filter notNull $ map (mapMaybe (filterOutEnemyUiMessages eid)) groups of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseOneFromEach x)
  ChooseN n msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseN n x)
  ChooseSome msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseSome x)
  ChooseSome1 doneMsg msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseSome1 doneMsg x)
  ChooseUpToN n msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseUpToN n x)
  ChooseOneAtATime msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseOneAtATime x)
  ChooseUpgradeDeck -> Just (Ask pid ChooseUpgradeDeck)
  ChooseDeck -> Just ask'
  ChoosePaymentAmounts {} -> Just ask'
  ChooseAmounts {} -> Just ask'
  PickScenarioSettings -> Just (Ask pid PickScenarioSettings)
  PickCampaignSettings -> Just (Ask pid PickCampaignSettings)
filterOutEnemyMessages eid msg = case msg of
  InitiateEnemyAttack details | eid == attackEnemy details -> Nothing
  EnemyAttack details | eid == attackEnemy details -> Nothing
  Discarded (EnemyTarget eid') _ _ | eid == eid' -> Nothing
  Do (Discarded (EnemyTarget eid') _ _) | eid == eid' -> Nothing
  PlaceEnemy eid' _ | eid' == eid -> Nothing
  m -> Just m

filterOutEnemyUiMessages :: EnemyId -> UI Message -> Maybe (UI Message)
filterOutEnemyUiMessages eid = \case
  TargetLabel (EnemyTarget eid') _ | eid == eid' -> Nothing
  EvadeLabel eid' _ | eid == eid' -> Nothing
  FightLabel eid' _ | eid == eid' -> Nothing
  other -> Just other

getInvestigatorsAtSameLocation :: HasGame m => EnemyAttrs -> m [InvestigatorId]
getInvestigatorsAtSameLocation attrs = do
  field EnemyLocation (toId attrs) >>= \case
    Nothing -> pure []
    Just loc -> select $ investigatorAt loc

getPreyMatcher :: HasGame m => EnemyAttrs -> m PreyMatcher
getPreyMatcher a = do
  mods <- getModifiers a
  pure $ foldl' applyModifier (enemyPrey a) mods
 where
  applyModifier _ (ForcePrey p) = p
  applyModifier p _ = p

isSwarm :: EnemyAttrs -> Bool
isSwarm attrs = case enemyPlacement attrs of
  AsSwarm {} -> True
  _ -> False

getCanReady :: HasGame m => EnemyAttrs -> m Bool
getCanReady a = do
  mods <- getModifiers a
  phase <- getPhase
  pure $ CannotReady `notElem` mods && (DoesNotReadyDuringUpkeep `notElem` mods || phase /= #upkeep)

getCanEngage :: HasGame m => EnemyAttrs -> m Bool
getCanEngage a = do
  keywords <- getModifiedKeywords a
  unengaged <- selectNone $ investigatorEngagedWith a.id
  pure $ all (`notElem` keywords) [#aloof, #massive] && unengaged

getAvailablePrey :: HasGame m => EnemyAttrs -> m [InvestigatorId]
getAvailablePrey a = do
  enemyLocation <- field EnemyLocation a.id
  iids <- fromMaybe [] <$> traverse (select . investigatorAt) enemyLocation
  if null iids
    then pure []
    else do
      getCanEngage a >>= \case
        False -> pure []
        True -> do
          let valids = mapOneOf InvestigatorWithId iids
          getPreyMatcher a >>= \case
            Prey m -> do
              preyIds <- select $ Prey $ m <> valids
              pure $ if null preyIds then iids else preyIds
            OnlyPrey m -> select $ OnlyPrey $ m <> valids
            other@(BearerOf {}) -> do
              mBearer <- selectOne other
              pure $ maybe [] (\bearer -> [bearer | bearer `elem` iids]) mBearer
            other@(RestrictedBearerOf {}) -> do
              mBearer <- selectOne other
              pure $ maybe [] (\bearer -> [bearer | bearer `elem` iids]) mBearer

instance RunMessage EnemyAttrs where
  runMessage msg a@EnemyAttrs {..} = case msg of
    UpdateEnemy eid upd | eid == enemyId -> do
      -- TODO: we may want life cycles around this, generally this might just be a bad idea
      pure $ updateEnemy [upd] a
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    EndPhase -> pure $ a & movedFromHunterKeywordL .~ False
    SealedChaosToken token (isTarget a -> True) -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    SealedChaosToken token _ -> do
      pure $ a & sealedChaosTokensL %~ filter (/= token)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    RemoveAllChaosTokens face -> pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      preyIds <- select =<< getPreyMatcher a
      runMessage (EnemySpawnEngagedWith eid $ oneOf $ map InvestigatorWithId preyIds) a
    EnemySpawnEngagedWith eid investigatorMatcher | eid == enemyId -> do
      preyIds <- select investigatorMatcher
      iidsWithLocations <- forToSnd preyIds (selectJust . locationWithInvestigator)
      lead <- getLeadPlayer
      push
        $ chooseOrRunOne
          lead
          [targetLabel iid $ resolve (EnemySpawn (Just iid) lid eid) | (iid, lid) <- toList iidsWithLocations]
      pure a
    SetBearer (EnemyTarget eid) iid | eid == enemyId -> do
      pure $ a & bearerL ?~ iid
    PlacedSwarmCard eid card | eid == enemyId -> do
      case toCard a of
        EncounterCard ec ->
          pushM $ createEnemyWithPlacement_ (EncounterCard $ ec {ecId = card.id}) (AsSwarm eid card)
        PlayerCard pc ->
          pushM $ createEnemyWithPlacement_ (PlayerCard $ pc {pcId = card.id}) (AsSwarm eid card)
        VengeanceCard _ -> error "not valid"
      pure a
    EnemySpawn miid lid eid | eid == enemyId -> do
      locations' <- select $ IncludeEmptySpace Anywhere
      canEnter <- eid <=~> IncludeOmnipotent (EnemyCanSpawnIn $ IncludeEmptySpace $ LocationWithId lid)
      if lid `notElem` locations' || not canEnter
        then push (toDiscard GameSource eid)
        else do
          keywords <- getModifiedKeywords a
          canSwarm <- withoutModifier a NoInitialSwarm
          let swarms = guard canSwarm *> mapMaybe (preview _Swarming) (toList keywords)

          case swarms of
            [] -> pure ()
            [x] -> do
              n <- getGameValue x
              lead <- getLead
              push $ PlaceSwarmCards lead eid n
            _ -> error "more than one swarming value"

          if all (`notElem` keywords) [#aloof, #massive] && not enemyExhausted
            then do
              prey <- getPreyMatcher a
              let
                onlyPrey = case prey of
                  Prey {} -> False
                  _ -> True
              preyIds <- select (preyWith prey $ investigatorAt lid)
              case miid of
                Just iid | not onlyPrey || iid `elem` preyIds -> do
                  atSameLocation <- iid <=~> investigatorAt lid
                  pushAll $ EnemyEntered eid lid : [EnemyEngageInvestigator eid iid | atSameLocation]
                _ -> do
                  investigatorIds <- if null preyIds then select $ investigatorAt lid else pure []
                  lead <- getLeadPlayer
                  let allIds = preyIds <> investigatorIds
                  let
                    validInvestigatorIds =
                      case miid of
                        Nothing -> allIds
                        Just iid -> if iid `elem` allIds then [iid] else allIds
                  case validInvestigatorIds of
                    [] -> push $ EnemyEntered eid lid
                    [iid] -> do
                      pushAll $ EnemyEntered eid lid
                        : [EnemyEngageInvestigator eid iid | not onlyPrey || iid `elem` preyIds]
                    iids -> do
                      let scoped = if not onlyPrey then iids else filter (`elem` preyIds) iids
                      case scoped of
                        [] -> push $ EnemyEntered eid lid
                        choices ->
                          push
                            $ chooseOne lead
                            $ [targetLabel iid [EnemyEntered eid lid, EnemyEngageInvestigator eid iid] | iid <- choices]
            else pushWhen (#massive `notElem` keywords) $ EnemyEntered eid lid

          when (#massive `elem` keywords) do
            investigatorIds <- select $ investigatorAt lid
            pushAll $ EnemyEntered eid lid : [EnemyEngageInvestigator eid iid | iid <- investigatorIds]
      pure a
    EnemyEntered eid lid | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyEntered eid' lid
          pure a
        _ -> do
          swarm <- select $ SwarmOf eid
          pushAll
            =<< traverse
              (\eid' -> checkWindows (($ Window.EnemyEnters eid' lid) <$> [mkAfter]))
              (eid : swarm)
          pushAll
            =<< traverse
              (\eid' -> checkWindows (($ Window.EnemyEnters eid' lid) <$> [mkWhen]))
              (eid : swarm)
          case a.placement of
            InThreatArea {} -> pure a
            _ -> pure $ a & placementL .~ AtLocation lid
    Ready (isTarget a -> True) -> do
      whenM (getCanReady a) do
        wouldDo msg (Window.WouldReady $ toTarget a) (Window.Readies $ toTarget a)
      pure a
    Do (Ready (isTarget a -> True)) -> do
      getCanReady a >>= \case
        False -> pure a
        True -> do
          case enemyPlacement of
            AsSwarm eid' _ -> do
              others <- select $ SwarmOf eid' <> not_ (be a) <> ExhaustedEnemy
              pushAll $ map (Ready . toTarget) others
            _ -> do
              others <- select $ SwarmOf a.id <> ExhaustedEnemy
              pushAll $ map (Ready . toTarget) others

          preyIds <- getAvailablePrey a
          unless (null preyIds) $ do
            lead <- getLeadPlayer
            push $ chooseOrRunOne lead $ targetLabels preyIds (only . EnemyEngageInvestigator enemyId)
          pure $ a & exhaustedL .~ False
    ReadyExhausted | not enemyDefeated -> do
      mods <- getModifiers a
      -- swarm will be readied by host
      case [source | AlternativeReady source <- mods] of
        [] ->
          when (enemyExhausted && DoesNotReadyDuringUpkeep `notElem` mods && not (isSwarm a))
            $ pushAll (resolve $ Ready $ toTarget a)
        [source] -> push (ReadyAlternative source (toTarget a))
        _ -> error "Can not handle multiple targets yet"
      pure a
    MoveToward target locationMatcher | isTarget a target -> do
      case enemyPlacement of
        AsSwarm eid' _ -> push $ MoveToward (EnemyTarget eid') locationMatcher
        _ -> do
          enemyLocation <- field EnemyLocation enemyId
          for_ enemyLocation $ \loc -> do
            lid <- fromJustNote "can't move toward" <$> selectOne locationMatcher
            when (lid /= loc) $ do
              lead <- getLeadPlayer
              adjacentLocationIds <-
                select $ AccessibleFrom $ LocationWithId loc
              closestLocationIds <- select $ ClosestPathLocation loc lid
              if lid `elem` adjacentLocationIds
                then push $ chooseOne lead [targetLabel lid [EnemyMove enemyId lid]]
                else
                  pushAll [chooseOne lead [targetLabel lid' [EnemyMove enemyId lid'] | lid' <- closestLocationIds]]
      pure a
    MoveUntil lid target | isTarget a target -> do
      case enemyPlacement of
        AsSwarm eid' _ -> push $ MoveUntil lid (EnemyTarget eid')
        _ -> do
          enemyLocation <- field EnemyLocation enemyId
          for_ enemyLocation \loc -> when (lid /= loc) do
            lead <- getLeadPlayer
            adjacentLocationIds <- select $ AccessibleFrom $ LocationWithId loc
            closestLocationIds <- select $ ClosestPathLocation loc lid
            if lid `elem` adjacentLocationIds
              then push $ chooseOne lead [targetLabel lid [EnemyMove enemyId lid]]
              else when (notNull closestLocationIds) do
                pushAll
                  [ chooseOne lead $ targetLabels closestLocationIds (only . EnemyMove enemyId)
                  , MoveUntil lid target
                  ]
      pure a
    Move movement | isTarget a (moveTarget movement) -> do
      case moveDestination movement of
        ToLocation destinationLocationId -> case moveMeans movement of
          Direct -> push $ EnemyMove (toId a) destinationLocationId
          OneAtATime -> push $ MoveUntil destinationLocationId (toTarget a)
          Towards -> push $ MoveToward (toTarget a) (LocationWithId destinationLocationId)
        ToLocationMatching matcher -> do
          lids <- select matcher
          player <- getLeadPlayer
          push
            $ chooseOrRunOne player
            $ [targetLabel lid [Move $ movement {moveDestination = ToLocation lid}] | lid <- lids]
      pure a
    EnemyMove eid lid | eid == enemyId -> case enemyPlacement of
      AsSwarm eid' _ -> do
        push $ EnemyMove eid' lid
        pure a
      _ -> do
        willMove <- canEnterLocation eid lid
        if willMove
          then do
            enemyLocation <- field EnemyLocation enemyId
            let leaveWindows = join $ map (\oldId -> windows [Window.EnemyLeaves eid oldId]) (maybeToList enemyLocation)
            pushAll $ leaveWindows <> [EnemyEntered eid lid, EnemyCheckEngagement eid]
            pure $ a & placementL .~ AtLocation lid
          else a <$ push (EnemyCheckEngagement eid)
    After (EndTurn _) | not enemyDefeated -> a <$ push (EnemyCheckEngagement $ toId a)
    EnemyCheckEngagement eid | eid == enemyId && not (isSwarm a) && not enemyDelayEngagement -> do
      let isAttached = isJust a.placement.attachedTo

      unless isAttached do
        keywords <- getModifiedKeywords a
        mods <- getModifiers eid
        let
          modifiedFilter iid = do
            if #massive `elem` keywords
              then pure True
              else do
                investigatorMods <- getModifiers iid
                canEngage <- flip allM investigatorMods $ \case
                  CannotBeEngagedBy matcher -> notElem eid <$> select matcher
                  CannotBeEngaged -> pure False
                  _ -> pure True
                pure $ canEngage && all (`notElem` mods) [EnemyCannotEngage iid, CannotBeEngaged]
        investigatorIds' <- filterM modifiedFilter =<< getInvestigatorsAtSameLocation a
        let valids = oneOf (map InvestigatorWithId investigatorIds')

        prey <- getPreyMatcher a
        investigatorIds <- case prey of
          Prey m -> do
            preyIds <- select $ Prey $ m <> valids
            pure $ if null preyIds then investigatorIds' else preyIds
          OnlyPrey m -> select $ OnlyPrey $ m <> valids
          other@(BearerOf {}) -> do
            mBearer <- selectOne other
            pure $ maybe [] (\bearer -> [bearer | bearer `elem` investigatorIds']) mBearer
          other@(RestrictedBearerOf {}) -> do
            mBearer <- selectOne other
            pure $ maybe [] (\bearer -> [bearer | bearer `elem` investigatorIds']) mBearer

        lead <- getLeadPlayer
        unengaged <- selectNone $ investigatorEngagedWith enemyId
        when (CannotBeEngaged `elem` mods) $ case enemyPlacement of
          InThreatArea iid -> push $ DisengageEnemy iid enemyId
          _ -> pure ()
        when
          ( none (`elem` keywords) [#aloof, #massive]
              && unengaged
              && CannotBeEngaged
              `notElem` mods
              && not enemyExhausted
          )
          $ if #massive `elem` keywords
            then
              pushAll
                [ EnemyEngageInvestigator eid investigatorId
                | investigatorId <- investigatorIds
                ]
            else case investigatorIds of
              [] -> pure ()
              [x] -> push $ EnemyEngageInvestigator eid x
              xs ->
                push
                  $ chooseOne lead
                  $ targetLabels xs (only . EnemyEngageInvestigator eid)
      pure a
    HuntersMove | not enemyExhausted && not (isSwarm a) && not enemyDefeated -> do
      -- TODO: unengaged or not engaged with only prey
      --
      let isAttached = isJust a.placement.attachedTo
      unless isAttached do
        wantsToMove <- selectNone $ InvestigatorAt (locationWithEnemy enemyId)
        mods <- getModifiers enemyId
        when (wantsToMove && CannotMove `notElem` mods) $ do
          keywords <- getModifiedKeywords a
          when (Keyword.Hunter `elem` keywords) do
            pushAll
              [ CheckWindows [mkWhen $ Window.MovedFromHunter enemyId]
              , HunterMove (toId a)
              ]
          -- We should never have a case where an enemy has both patrol and
          -- hunter and should only have one patrol keyword
          for_ keywords \case
            Keyword.Patrol lMatcher -> push $ PatrolMove (toId a) lMatcher
            _ -> pure ()
      pure a
    SwapPlaces (aTarget, _) (_, newLocation) | a `is` aTarget -> do
      push $ EnemyCheckEngagement a.id
      pure $ a & placementL .~ AtLocation newLocation
    SwapPlaces (_, newLocation) (bTarget, _) | a `is` bTarget -> do
      push $ EnemyCheckEngagement a.id
      pure $ a & placementL .~ AtLocation newLocation
    HunterMove eid | eid == toId a && not enemyExhausted && not (isSwarm a) -> do
      field EnemyLocation enemyId >>= \case
        Nothing -> pure a
        Just loc -> do
          mods <- getModifiers enemyId
          let
            locationMatcherModifier = if CanEnterEmptySpace `elem` mods then IncludeEmptySpace else id
            matchForcedTargetLocation = \case
              DuringEnemyPhaseMustMoveToward (LocationTarget lid) -> Just lid
              _ -> Nothing
            forcedTargetLocation = firstJust matchForcedTargetLocation mods
            additionalConnections = [ConnectedToWhen (LocationWithId loc) (LocationWithId lid') | HunterConnectedTo lid' <- mods]

          -- applyConnectionMapModifier connectionMap (HunterConnectedTo lid') =
          --  unionWith (<>) connectionMap $ singletonMap loc [lid']
          -- applyConnectionMapModifier connectionMap _ = connectionMap
          -- extraConnectionsMap :: Map LocationId [LocationId] = foldl' applyConnectionMapModifier mempty mods

          enemiesAsInvestigatorLocations <-
            withModifiers loc (toModifiers a additionalConnections)
              $ select
              $ locationMatcherModifier
              $ LocationWithEnemy
              $ NearestEnemyToLocation loc
              $ EnemyWithModifier CountsAsInvestigatorForHunterEnemies

          -- The logic here is an artifact of doing this incorrect
          -- Prey is only used for breaking ties unless we're dealing
          -- with the Only keyword for prey, so here we hardcode prey
          -- to AnyPrey and then find if there are any investigators
          -- who qualify as prey to filter
          prey <- getPreyMatcher a
          matchingClosestLocationIds <- withModifiers loc (toModifiers a additionalConnections)
            $ case (forcedTargetLocation, prey) of
              (Just forcedTargetLocationId, _) ->
                -- Lure (1)
                select $ locationMatcherModifier $ ClosestPathLocation loc forcedTargetLocationId
              (Nothing, BearerOf _) ->
                select
                  $ locationMatcherModifier
                  $ locationWithInvestigator
                  $ fromJustNote "must have bearer" enemyBearer
              (Nothing, RestrictedBearerOf _ _) -> do
                -- this case should never happen, but just in case
                select
                  $ locationMatcherModifier
                  $ locationWithInvestigator
                  $ fromJustNote "must have bearer" enemyBearer
              (Nothing, OnlyPrey onlyPrey) ->
                select $ locationMatcherModifier $ LocationWithInvestigator $ onlyPrey <> NearestToEnemy (be eid)
              (Nothing, _prey) -> do
                investigatorLocations <-
                  select
                    $ locationMatcherModifier
                    $ LocationWithInvestigator
                    $ NearestToEnemy (be eid)
                    <> CanBeHuntedBy eid
                select
                  $ locationMatcherModifier
                  $ NearestLocationToLocation
                    loc
                    (mapOneOf LocationWithId $ enemiesAsInvestigatorLocations <> investigatorLocations)

          preyIds <- select prey
          let includeEnemies = prey == Prey Anyone

          filteredClosestLocationIds <-
            flip filterM matchingClosestLocationIds $ \lid -> do
              hasInvestigators <-
                notNull
                  . List.intersect preyIds
                  <$> select (InvestigatorAt (locationMatcherModifier $ LocationWithId lid))
              hasEnemies <-
                notNull
                  <$> select
                    ( EnemyAt (locationMatcherModifier $ LocationWithId lid)
                        <> EnemyWithModifier CountsAsInvestigatorForHunterEnemies
                    )
              pure $ hasInvestigators || (includeEnemies && hasEnemies)

          -- If we have any locations with prey, that takes priority, otherwise
          -- we return all locations which may have matched via AnyPrey
          let
            destinationLocationIds =
              if null filteredClosestLocationIds
                then matchingClosestLocationIds
                else filteredClosestLocationIds

          lead <- getLeadPlayer
          pathIds' <- withModifiers loc (toModifiers a additionalConnections) do
            concatForM destinationLocationIds
              $ select
              . locationMatcherModifier
              . (LocationCanBeEnteredBy enemyId <>)
              . ClosestPathLocation loc

          pathIds <- withModifiers loc (toModifiers a additionalConnections) do
            if CanIgnoreBarriers `elem` mods
              then do
                barricadedPathIds <-
                  concatForM destinationLocationIds
                    $ select
                    . locationMatcherModifier
                    . (LocationCanBeEnteredBy enemyId <>)
                    . ClosestUnbarricadedPathLocation loc
                pure $ if null barricadedPathIds then pathIds' else barricadedPathIds
              else pure pathIds'

          case pathIds of
            [] -> pure a
            [lid] -> do
              pushAll
                [ EnemyMove enemyId lid
                , CheckWindows [mkAfter $ Window.MovedFromHunter enemyId]
                , CheckWindows [mkAfter $ Window.EnemyMovesTo lid MovedViaHunter enemyId]
                ]
              pure $ a & movedFromHunterKeywordL .~ True
            ls -> do
              push
                $ chooseOrRunOne
                  lead
                  [ targetLabel
                    l
                    [ EnemyMove enemyId l
                    , CheckWindows [mkAfter $ Window.MovedFromHunter enemyId]
                    , CheckWindows [mkAfter $ Window.EnemyMovesTo l MovedViaHunter enemyId]
                    ]
                  | l <- ls
                  ]
              pure $ a & movedFromHunterKeywordL .~ True
    PatrolMove eid lMatcher | eid == toId a && not enemyExhausted && not (isSwarm a) -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> do
          mods <- getModifiers enemyId
          let locationMatcherModifier = if CanEnterEmptySpace `elem` mods then IncludeEmptySpace else id

          destinationLocationIds <-
            select $ locationMatcherModifier $ NearestLocationToLocation loc lMatcher

          lead <- getLeadPlayer
          pathIds <-
            concatForM destinationLocationIds (select . locationMatcherModifier . ClosestPathLocation loc)
          case pathIds of
            [] -> pure ()
            [lid] -> do
              pushAll
                [ EnemyMove enemyId lid
                , -- , CheckWindow
                  --     [leadInvestigatorId]
                  --     [mkWindow Timing.After (Window.MovedFromHunter enemyId)]
                  CheckWindows [mkAfter $ Window.EnemyMovesTo lid MovedViaOther enemyId]
                ]
            ls -> do
              push
                $ chooseOrRunOne
                  lead
                  [ targetLabel
                    l
                    [ EnemyMove enemyId l
                    , -- , CheckWindow
                      --     [leadInvestigatorId]
                      --     [mkWindow Timing.After (Window.MovedFromHunter enemyId)]
                      CheckWindows [mkAfter $ Window.EnemyMovesTo l MovedViaOther enemyId]
                    ]
                  | l <- ls
                  ]
          pure a
    EnemiesAttack | not enemyExhausted && not enemyDefeated -> do
      mods <- getModifiers (EnemyTarget enemyId)
      unless (CannotAttack `elem` mods) do
        iids <- select enemyAttacks
        for_ iids \iid ->
          push
            $ EnemyWillAttack
            $ (enemyAttack enemyId a iid)
              { attackDamageStrategy = enemyDamageStrategy
              , attackExhaustsEnemy = True
              }
      pure a
    AttackEnemy sid iid eid source mTarget skillType | eid == enemyId -> do
      whenWindow <- checkWindows [mkWhen (Window.EnemyAttacked iid source enemyId)]
      afterWindow <- checkWindows [mkAfter (Window.EnemyAttacked iid source enemyId)]
      keywords <- getModifiedKeywords a

      pushWhen (Keyword.Elusive `elem` keywords) $ HandleElusive eid

      pushAll
        [ whenWindow
        , fight
            sid
            iid
            source
            (maybe (toTarget eid) (ProxyTarget (toTarget eid)) mTarget)
            skillType
            (EnemyMaybeFieldCalculation eid EnemyFight)
        , afterWindow
        ]
      pure a
    HandleElusive eid | eid == enemyId -> do
      -- just a reminder that the messages are handled in reverse, so exhaust happens last
      when (isInPlayPlacement enemyPlacement) do
        whenM (eid <=~> ReadyEnemy) do
          push $ Exhaust (toTarget a)

          emptyConnectedLocations <-
            select $ ConnectedFrom (locationWithEnemy eid) <> not_ (LocationWithInvestigator Anyone)
          lead <- getLeadPlayer
          if notNull emptyConnectedLocations
            then do
              push $ chooseOrRunOne lead [targetLabel lid [EnemyMove eid lid] | lid <- emptyConnectedLocations]
            else do
              otherConnectedLocations <-
                select $ ConnectedFrom (locationWithEnemy eid) <> LocationWithInvestigator Anyone
              when (notNull otherConnectedLocations) do
                push $ chooseOrRunOne lead [targetLabel lid [EnemyMove eid lid] | lid <- otherConnectedLocations]

          push $ DisengageEnemyFromAll eid
      pure a
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isActionTarget a target -> do
      whenWindow <- checkWindows [mkWhen (Window.SuccessfulAttackEnemy iid source enemyId n)]
      afterSuccessfulWindow <- checkWindows [mkAfter (Window.SuccessfulAttackEnemy iid source enemyId n)]
      pushAll
        [ whenWindow
        , Successful (Action.Fight, toProxyTarget target) iid source (toActionTarget target) n
        , afterSuccessfulWindow
        ]

      pure a
    Successful (Action.Fight, _) iid source target n | isTarget a target -> do
      mods <- getModifiers a
      let alternateSuccess = [t | AlternateSuccess t <- mods]
      pushWhen (null alternateSuccess) $ InvestigatorDamageEnemy iid enemyId source
      for_ alternateSuccess $ \target' ->
        push $ Successful (Action.Fight, toTarget a) iid source target' n
      pure a
    FailedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isTarget a target -> do
      pushAll
        [ FailedAttackEnemy iid enemyId
        , CheckWindows [mkAfter $ Window.FailAttackEnemy iid enemyId n]
        , CheckWindows [mkAfter $ Window.EnemyAttacked iid source enemyId]
        , Failed (Action.Fight, toProxyTarget target) iid source (toActionTarget target) n
        ]
      pure a
    Failed (Action.Fight, _) iid _source target _ | isTarget a target -> do
      mods <- getModifiers iid
      keywords <- getModifiedKeywords a
      pushAll
        [ EnemyAttack $ (enemyAttack enemyId a iid) {attackDamageStrategy = enemyDamageStrategy}
        | Keyword.Retaliate `elem` keywords
        , IgnoreRetaliate `notElem` mods
        , not enemyExhausted || CanRetaliateWhileExhausted `elem` mods
        ]
      pure a
    EnemyAttackIfEngaged eid miid | eid == enemyId -> do
      case miid of
        Just iid -> do
          shouldAttack <- elem iid <$> select (investigatorEngagedWith eid)
          when shouldAttack do
            push $ EnemyAttack $ (enemyAttack enemyId a iid) {attackDamageStrategy = enemyDamageStrategy}
        Nothing -> do
          iids <- select $ investigatorEngagedWith eid
          pushAll
            [ EnemyAttack $ (enemyAttack enemyId a iid) {attackDamageStrategy = enemyDamageStrategy} | iid <- iids
            ]
      pure a
    EnemyEvaded iid eid | eid == enemyId -> do
      whenWindow <- checkWindows [mkWhen $ Window.EnemyEvaded iid enemyId]
      afterWindow <- checkWindows [mkAfter $ Window.EnemyEvaded iid enemyId]
      pushAll [whenWindow, Do msg, afterWindow]
      pure a
    Do (EnemyEvaded iid eid) | eid == enemyId -> do
      mods <- getModifiers iid
      pushWhen (DoNotExhaustEvaded `notElem` mods) $ Exhaust (toTarget a)
      pushWhen (DoNotDisengageEvaded `notElem` mods) $ DisengageEnemyFromAll eid
      pure a
    Exhaust (isTarget a -> True) -> do
      afterWindow <- checkWindows [mkAfter $ Window.Exhausts (toTarget a)]
      push afterWindow
      case enemyPlacement of
        AsSwarm eid' _ -> push $ Exhaust (toTarget eid')
        _ -> do
          others <- select $ SwarmOf (toId a) <> ReadyEnemy
          pushAll [Exhaust (toTarget other) | other <- others]
      pure $ a & exhaustedL .~ True
    TryEvadeEnemy sid iid eid source mTarget skillType | eid == enemyId -> do
      mEnemyEvade' <- field EnemyEvade eid
      case mEnemyEvade' of
        Just _ ->
          push
            $ evade
              sid
              iid
              source
              (maybe (toTarget eid) (ProxyTarget (toTarget eid)) mTarget)
              skillType
              (EnemyMaybeFieldCalculation eid EnemyEvade)
        Nothing -> error "No evade value"
      pure a
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isActionTarget a target -> do
      whenWindow <- checkWindows [mkWhen $ Window.SuccessfulEvadeEnemy iid enemyId n]
      afterWindow <- checkWindows [mkAfter $ Window.SuccessfulEvadeEnemy iid enemyId n]
      pushAll
        [ whenWindow
        , Successful (Action.Evade, toProxyTarget target) iid source (toActionTarget target) n
        , afterWindow
        ]
      pure a
    Successful (Action.Evade, _) iid source target n | isTarget a target -> do
      mods <- getModifiers a
      let alternateSuccess = [t | AlternateSuccess t <- mods]
      pushWhen (null alternateSuccess) $ EnemyEvaded iid enemyId
      for_ alternateSuccess $ \target' ->
        push $ Successful (Action.Evade, toTarget a) iid source target' n
      pure a
    FailedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isActionTarget a target -> do
      whenWindow <- checkWindows [mkWhen $ Window.FailEvadeEnemy iid enemyId n]
      afterWindow <- checkWindows [mkAfter $ Window.FailEvadeEnemy iid enemyId n]
      pushAll
        [ whenWindow
        , Failed (Action.Evade, toProxyTarget target) iid source (toActionTarget target) n
        , afterWindow
        ]
      pure a
    Failed (Action.Evade, _) iid _ target _ | isTarget a target -> do
      mods <- getModifiers iid
      keywords <- getModifiedKeywords a
      pushAll
        [ EnemyAttack $ viaAlert $ (enemyAttack enemyId a iid) {attackDamageStrategy = enemyDamageStrategy}
        | Keyword.Alert `elem` keywords
        , IgnoreRetaliate `notElem` mods
        ]
      pure a
    InitiateEnemyAttack details | details.enemy == enemyId -> do
      mods <- getModifiers a
      let canBeCancelled = AttacksCannotBeCancelled `notElem` mods
      let strategy = fromMaybe details.strategy $ listToMaybe [s | SetAttackDamageStrategy s <- mods]
      push $ EnemyAttack $ details {attackCanBeCanceled = canBeCancelled, attackDamageStrategy = strategy}
      pure a
    ChangeEnemyAttackTarget eid target | eid == enemyId -> do
      let details = fromJustNote "missing attack details" enemyAttacking
          details' = details {attackTarget = target}
      replaceWindow
        \case
          (Window.windowType -> Window.EnemyAttacks d) -> d == details
          _ -> False
        \w -> w {Window.windowType = Window.EnemyAttacks details'}
      replaceWindow
        \case
          (Window.windowType -> Window.EnemyAttacksEvenIfCancelled d) -> d == details
          _ -> False
        \w -> w {Window.windowType = Window.EnemyAttacksEvenIfCancelled details'}
      pure $ a & attackingL ?~ details'
    ChangeEnemyAttackDetails eid details' | eid == enemyId -> do
      pure $ a & attackingL ?~ details'
    AfterEnemyAttack eid msgs | eid == enemyId -> do
      let details = fromJustNote "missing attack details" enemyAttacking
      pure $ a & attackingL ?~ details {attackAfter = msgs}
    EnemyAttack details | attackEnemy details == enemyId -> do
      case attackTarget details of
        InvestigatorTarget iid -> do
          canIgnore <- hasModifier iid MayIgnoreAttacksOfOpportunity
          willIgnore <- hasModifier iid IgnoreAttacksOfOpportunity
          if (canIgnore || willIgnore) && attackType details == AttackOfOpportunity
            then do
              player <- getPlayer iid
              when canIgnore do
                push $ chooseOne player [Label "Ignore attack of opportunity" [], Label "Do not ignore" [Do msg]]
            else push $ Do msg
        _ -> push $ Do msg
      pure a
    Do (EnemyAttack details) | attackEnemy details == enemyId -> do
      mods <- getModifiers a
      let canBeCancelled = AttacksCannotBeCancelled `notElem` mods
      let strategy =
            fromMaybe (attackDamageStrategy details)
              $ listToMaybe [s | SetAttackDamageStrategy s <- mods]
      whenAttacksWindow <- checkWindows [mkWhen $ Window.EnemyAttacks details]
      afterAttacksEventIfCancelledWindow <-
        checkWindows [mkAfter $ Window.EnemyAttacksEvenIfCancelled details]
      whenWouldAttackWindow <- checkWindows [mkWhen $ Window.EnemyWouldAttack details]
      pushAll
        [ whenWouldAttackWindow
        , whenAttacksWindow
        , PerformEnemyAttack enemyId
        , After (PerformEnemyAttack enemyId)
        , afterAttacksEventIfCancelledWindow
        ]
      pure
        $ a
        & attackingL
        ?~ details {attackCanBeCanceled = canBeCancelled, attackDamageStrategy = strategy}
    PerformEnemyAttack eid | eid == enemyId && not enemyDefeated -> do
      let details = fromJustNote "missing attack details" enemyAttacking
      modifiers <- getModifiers (attackTarget details)
      mods <- getModifiers a
      sourceModifiers <- maybe (pure []) getModifiers (sourceToMaybeTarget details.source)

      let
        applyModifiers cards (CancelAttacksByEnemies c n) = do
          canceled <- elem enemyId <$> select n
          pure $ if canceled then c : cards else cards
        applyModifiers m _ = pure m

      cardsThatCanceled <- foldM applyModifiers [] modifiers

      ignoreWindows <- for cardsThatCanceled \card ->
        checkWindows [mkAfter $ Window.CancelledOrIgnoredCardOrGameEffect $ CardIdSource card.id]

      let
        allowAttack =
          or
            [ null cardsThatCanceled
            , EffectsCannotBeCanceled `notElem` sourceModifiers || not (attackCanBeCanceled details)
            ]

      healthDamage <-
        if attackDealDamage details
          then field EnemyHealthDamage (toId a)
          else pure 0
      sanityDamage <- field EnemySanityDamage (toId a)

      case attackTarget details of
        InvestigatorTarget iid -> do
          player <- getPlayer iid
          let
            attackMessage =
              if AttackDealsEitherDamageOrHorror `elem` modifiers
                then
                  chooseOne
                    player
                    [ Label
                        ("Take " <> tshow healthDamage <> " damage")
                        [ InvestigatorAssignDamage
                            iid
                            (EnemyAttackSource enemyId)
                            (attackDamageStrategy details)
                            healthDamage
                            0
                        ]
                    , Label
                        ("Take " <> tshow sanityDamage <> " horror")
                        [ InvestigatorAssignDamage
                            iid
                            (EnemyAttackSource enemyId)
                            (attackDamageStrategy details)
                            0
                            sanityDamage
                        ]
                    ]
                else
                  InvestigatorAssignDamage
                    iid
                    (EnemyAttackSource enemyId)
                    (attackDamageStrategy details)
                    healthDamage
                    sanityDamage
          pushAll
            $ [attackMessage | allowAttack]
            <> [Exhaust (toTarget a) | allowAttack, attackExhaustsEnemy details, DoNotExhaust `notElem` mods]
            <> ignoreWindows
            <> [After (EnemyAttack details)]
        _ -> error "Unhandled"
      pure a
    After (EnemyAttack details) | details.enemy == a.id -> do
      for_ enemyAttacking \updatedDetails -> do
        keywords <- getModifiedKeywords a
        afterAttacksWindow <- checkAfter $ Window.EnemyAttacks updatedDetails
        pushWhen (Keyword.Elusive `elem` keywords) $ HandleElusive a.id
        pushAll $ afterAttacksWindow : attackAfter updatedDetails
        when (attackType details == AttackOfOpportunity) do
          case attackTarget details of
            InvestigatorTarget iid -> push $ UpdateHistory iid (HistoryItem HistoryAttacksOfOpportunity 1)
            _ -> pure ()
      pure a
    HealDamage (EnemyTarget eid) source n | eid == enemyId -> do
      afterWindow <- checkAfter $ Window.Healed DamageType (toTarget a) source n
      push afterWindow
      runMessage (RemoveTokens source (toTarget a) #damage n) a
    HealAllDamage (EnemyTarget eid) source | eid == enemyId -> do
      afterWindow <-
        checkWindows [mkAfter $ Window.Healed DamageType (toTarget a) source (enemyDamage a)]
      push afterWindow
      pure $ a & tokensL %~ removeAllTokens Token.Damage & defeatedL .~ False
    Msg.EnemyDamage eid damageAssignment | eid == enemyId -> do
      let
        source = damageAssignmentSource damageAssignment
        damageEffect = damageAssignmentDamageEffect damageAssignment
        damageAmount = damageAssignmentAmount damageAssignment
      canDamage <- sourceCanDamageEnemy eid source
      when canDamage do
        dealtDamageWhenMsg <-
          checkWindows [mkWhen $ Window.DealtDamage source damageEffect (toTarget a) damageAmount]
        dealtDamageAfterMsg <-
          checkWindows [mkAfter $ Window.DealtDamage source damageEffect (toTarget a) damageAmount]
        takeDamageWhenMsg <-
          checkWindows [mkWhen $ Window.TakeDamage source damageEffect (toTarget a) damageAmount]
        takeDamageAfterMsg <-
          checkWindows [mkAfter $ Window.TakeDamage source damageEffect (toTarget a) damageAmount]
        pushAll
          [ dealtDamageWhenMsg
          , dealtDamageAfterMsg
          , takeDamageWhenMsg
          , EnemyDamaged eid damageAssignment
          , takeDamageAfterMsg
          ]
      pure a
    EnemyDamaged eid damageAssignment | eid == enemyId -> do
      let source = damageAssignmentSource damageAssignment
      canDamage <- sourceCanDamageEnemy eid source
      if canDamage
        then do
          amount' <- getModifiedDamageAmount a damageAssignment
          let
            damageAssignment' = damageAssignment {damageAssignmentAmount = amount'}
            combine l r =
              if damageAssignmentDamageEffect l == damageAssignmentDamageEffect r
                then l {damageAssignmentAmount = damageAssignmentAmount l + damageAssignmentAmount r}
                else
                  error
                    $ "mismatched damage assignments\n\nassignment: "
                    <> show l
                    <> "\nnew assignment: "
                    <> show r
          unless (damageAssignmentDelayed damageAssignment')
            $ push
            $ checkDefeated source eid
          push $ AssignedDamage (toTarget a)
          pure
            $ a
            & assignedDamageL
            %~ insertWith combine source damageAssignment'
        else pure a
    CheckDefeated source (isTarget a -> True) -> do
      let mDamageAssignment = lookup source enemyAssignedDamage
      case mDamageAssignment of
        Nothing -> do
          hasSwarm <- selectAny $ SwarmOf (toId a)
          canBeDefeated <- withoutModifier a CannotBeDefeated
          modifiers' <- getModifiers (toTarget a)
          let
            eid = toId a
            canOnlyBeDefeatedByModifier = \case
              CanOnlyBeDefeatedBy source' -> First (Just source')
              _ -> First Nothing
            mOnlyBeDefeatedByModifier =
              getFirst $ foldMap canOnlyBeDefeatedByModifier modifiers'
          let validDefeat = canBeDefeated && not hasSwarm && isNothing mOnlyBeDefeatedByModifier
          when validDefeat $ do
            field EnemyHealth (toId a) >>= traverse_ \modifiedHealth -> do
              when (enemyDamage a >= modifiedHealth) $ do
                whenMsg <- checkWindows [mkWhen $ Window.EnemyWouldBeDefeated eid]
                afterMsg <- checkWindows [mkAfter $ Window.EnemyWouldBeDefeated eid]
                let
                  defeatMsgs =
                    if ExhaustIfDefeated `elem` modifiers'
                      then [Exhaust (toTarget a) | not enemyExhausted]
                      else [EnemyDefeated eid (toCardId a) source (setToList $ toTraits a)]

                pushAll $ [whenMsg, afterMsg] <> defeatMsgs
          pure a
        Just da -> do
          hasSwarm <- selectAny $ SwarmOf (toId a)
          canBeDefeated <- withoutModifier a CannotBeDefeated
          modifiers' <- getModifiers (toTarget a)
          let
            eid = toId a
            amount' = damageAssignmentAmount da
            damageEffect = damageAssignmentDamageEffect da
            canOnlyBeDefeatedByModifier = \case
              CanOnlyBeDefeatedBy source' -> First (Just source')
              _ -> First Nothing
            mOnlyBeDefeatedByModifier = getFirst $ foldMap canOnlyBeDefeatedByModifier modifiers'
          validDefeat <-
            ( ( canBeDefeated
                  && not hasSwarm
              )
                &&
              )
              <$> maybe (pure True) (sourceMatches source) mOnlyBeDefeatedByModifier
          when validDefeat $ do
            field EnemyHealth (toId a) >>= traverse_ \modifiedHealth -> do
              when (enemyDamage a + amount' >= modifiedHealth) $ do
                let excess = (enemyDamage a + amount') - modifiedHealth
                let
                  mSwarmOf = case enemyPlacement of
                    AsSwarm eid' _ -> Just eid'
                    _ -> Nothing
                controller <- maybe getLeadPlayer getPlayer =<< getSourceController source

                excessDamageTargets <- case mSwarmOf of
                  Nothing -> pure []
                  Just eid' -> (eid' :) <$> select (not_ (EnemyWithId $ toId a) <> SwarmOf eid')

                whenMsg <- checkWindows [mkWhen $ Window.EnemyWouldBeDefeated eid]
                afterMsg <- checkWindows [mkAfter $ Window.EnemyWouldBeDefeated eid]
                whenExcessMsg <-
                  checkWindows
                    [mkWhen $ Window.DealtExcessDamage source damageEffect (toTarget eid) excess | excess > 0]
                afterExcessMsg <-
                  checkWindows
                    [mkAfter $ Window.DealtExcessDamage source damageEffect (toTarget eid) excess | excess > 0]

                let
                  defeatMsgs =
                    if ExhaustIfDefeated `elem` modifiers'
                      then [Exhaust (toTarget a) | not enemyExhausted]
                      else
                        [EnemyDefeated eid (toCardId a) source (setToList $ toTraits a)]
                          <> ( guard (notNull excessDamageTargets && excess > 0)
                                *> [ ExcessDamage
                                      eid
                                      [ chooseOne
                                          controller
                                          [ Label
                                              "Deal Excess Damage to Host or Swarm?"
                                              [ chooseOrRunOne
                                                  controller
                                                  [ targetLabel other [Msg.EnemyDamage other (da {damageAssignmentAmount = excess})]
                                                  | other <- excessDamageTargets
                                                  ]
                                              ]
                                          , Label "Do not deal excess damage" $ map (CheckDefeated GameSource . toTarget) (toList mSwarmOf)
                                          ]
                                      ]
                                   ]
                             )

                pushAll $ [whenExcessMsg, afterExcessMsg, whenMsg, afterMsg] <> defeatMsgs
          pure $ a & assignedDamageL .~ mempty & tokensL . at #damage . non 0 +~ amount'
    DefeatEnemy eid _ source | eid == enemyId -> do
      canBeDefeated <- withoutModifier a CannotBeDefeated
      modifiedHealth <- fieldJust EnemyHealth (toId a)
      canOnlyBeDefeatedByDamage <- hasModifier a CanOnlyBeDefeatedByDamage
      modifiers' <- getModifiers (toTarget a)
      let
        defeatedByDamage = enemyDamage a >= modifiedHealth
        canOnlyBeDefeatedByModifier = \case
          CanOnlyBeDefeatedBy source' -> First (Just source')
          _ -> First Nothing
        mOnlyBeDefeatedByModifier =
          getFirst $ foldMap canOnlyBeDefeatedByModifier modifiers'
      validDefeat <-
        ( ( canBeDefeated
              && (not canOnlyBeDefeatedByDamage || defeatedByDamage)
          )
            &&
          )
          <$> maybe (pure True) (sourceMatches source) mOnlyBeDefeatedByModifier
      when validDefeat do
        push $ EnemyDefeated eid (toCardId a) source (setToList $ toTraits a)
      pure a
    EnemyDefeated eid _ source _ | eid == toId a -> do
      modifiedHealth <- fieldJust EnemyHealth (toId a)
      let
        defeatedByDamage = enemyDamage a >= modifiedHealth
        defeatedBy = if defeatedByDamage then DefeatedByDamage source else DefeatedByOther source
      miid <- getSourceController source
      whenMsg <- checkWindows [mkWhen $ Window.EnemyDefeated miid defeatedBy eid]
      afterMsg <- checkWindows [mkAfter $ Window.EnemyDefeated miid defeatedBy eid]
      victory <- getVictoryPoints eid
      mloc <- field EnemyLocation a.id
      vengeance <- getVengeancePoints eid
      let
        victoryMsgs = [DefeatedAddToVictory $ toTarget a | isJust (victory <|> vengeance)]
        defeatMsgs =
          if isJust (victory <|> vengeance)
            then resolve $ RemoveEnemy eid
            else [Discard miid GameSource $ toTarget a]

      withQueue_ $ mapMaybe (filterOutEnemyMessages eid)

      pushAll
        $ [whenMsg, When msg, After msg]
        <> ( case miid of
              Just iid -> [PlaceKey (toTarget iid) ekey | ekey <- toList enemyKeys]
              Nothing -> case mloc of
                Just lid -> [PlaceKey (toTarget lid) ekey | ekey <- toList enemyKeys]
                _ -> []
           )
        <> victoryMsgs
        <> [afterMsg]
        <> windows [Window.EntityDiscarded source (toTarget a)]
        <> defeatMsgs
      pure $ a & keysL .~ mempty
    After (EnemyDefeated eid _ source _) | eid == toId a -> do
      case a.placement of
        AsSwarm eid' _ -> push $ CheckDefeated source (toTarget eid')
        _ -> pure ()
      pure $ a & defeatedL .~ True
    Discard miid source target | a `isTarget` target -> do
      whenLeavePlay <- checkWindows [mkWhen $ Window.LeavePlay (toTarget a)]
      afterLeavePlay <- checkWindows [mkWhen $ Window.LeavePlay (toTarget a)]
      let
        card = case enemyPlacement of
          AsSwarm _ c -> c
          _ -> toCard a
      pushAll
        $ windows [Window.WouldBeDiscarded (toTarget a)]
        <> windows [Window.EntityDiscarded source (toTarget a)]
        <> [ whenLeavePlay
           , RemovedFromPlay $ toSource a
           , afterLeavePlay
           , Discarded (toTarget a) source card
           , Do (Discarded (toTarget a) source card)
           ]
      pure $ a & keysL .~ mempty & discardedByL .~ miid
    PutOnTopOfDeck iid deck target | a `isTarget` target -> do
      pushAll
        $ resolve (RemoveEnemy $ toId a)
        <> [PutCardOnTopOfDeck iid deck (toCard a)]
      pure a
    PutOnBottomOfDeck iid deck target | a `isTarget` target -> do
      pushAll
        $ resolve (RemoveEnemy $ toId a)
        <> [PutCardOnBottomOfDeck iid deck (toCard a)]
      pure a
    RemovedFromPlay source | isSource a source -> do
      enemyAssets <- select $ EnemyAsset enemyId
      pushAll
        $ map (toDiscard GameSource) enemyAssets
        <> [UnsealChaosToken token | token <- enemySealedChaosTokens]
      pure a
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      runMessage (EngageEnemy iid eid Nothing False) a
    EngageEnemy iid eid mTarget False | eid == enemyId -> do
      eliminated <- selectNone $ InvestigatorWithId iid
      if eliminated
        then push $ EnemyCheckEngagement eid
        else do
          let (before, _, after) = frame (Window.EnemyEngaged iid eid)
          case enemyPlacement of
            AsSwarm eid' _ -> do
              pushAll
                [ before
                , EngageEnemy iid eid' mTarget False
                , after
                ]
            _ -> do
              massive <- eid <=~> MassiveEnemy
              mlid <- getMaybeLocation iid
              enemyLocation <- field EnemyLocation eid
              when (not massive) do
                pushAll
                  $ [before, PlaceEnemy eid (InThreatArea iid)]
                  <> [EnemyEntered eid lid | lid <- maybeToList mlid, Just lid /= enemyLocation]
                  <> [after]
      pure a
    WhenWillEnterLocation iid lid -> do
      case enemyPlacement of
        InThreatArea iid' | iid' == iid -> do
          keywords <- getModifiedKeywords a
          willMove <- canEnterLocation enemyId lid
          -- TODO: we may not need to check massive anymore since we look at placement
          if #massive `notElem` keywords && willMove
            then push $ EnemyEntered enemyId lid
            else push $ DisengageEnemy iid enemyId
        _ -> pure ()
      pure a
    InvestigatorDamage iid (EnemyAttackSource eid) x y | eid == enemyId -> do
      pure $ a & attackingL . _Just . damagedL . at (toTarget iid) . non (0, 0) %~ bimap (+ x) (+ y)
    AssignAssetDamageWithCheck aid (EnemyAttackSource eid) x y _ | eid == enemyId -> do
      pure $ a & attackingL . _Just . damagedL . at (toTarget aid) . non (0, 0) %~ bimap (+ x) (+ y)
    CancelAssetDamage aid (EnemyAttackSource eid) x | eid == enemyId -> do
      pure
        $ a
        & (attackingL . _Just . damagedL . at (toTarget aid) . non (0, 0) %~ first (max 0 . subtract x))
    CheckAttackOfOpportunity iid isFast | not isFast && not enemyExhausted -> do
      willAttack <- elem iid <$> select (investigatorEngagedWith enemyId)
      when willAttack $ do
        modifiers' <- getModifiers enemyId
        unless (any (`elem` modifiers') [CannotMakeAttacksOfOpportunity, CannotAttack])
          $ push
          $ EnemyWillAttack
          $ EnemyAttackDetails
            { attackEnemy = enemyId
            , attackTarget = InvestigatorTarget iid
            , attackOriginalTarget = InvestigatorTarget iid
            , attackDamageStrategy = enemyDamageStrategy
            , attackType = AttackOfOpportunity
            , attackExhaustsEnemy = False
            , attackSource = toSource a
            , attackCanBeCanceled = True
            , attackAfter = []
            , attackDamaged = mempty
            , attackDealDamage = True
            }
      pure a
    InvestigatorDrawEnemy iid eid | eid == enemyId -> do
      mods <- (<>) <$> getModifiers enemyId <*> getModifiers (CardIdTarget $ toCardId a)
      let
        getModifiedSpawnAt [] = enemySpawnAt
        getModifiedSpawnAt (ForceSpawnLocation m : _) = Just $ SpawnAt m
        getModifiedSpawnAt (ForceSpawn m : _) = Just m
        getModifiedSpawnAt (_ : xs) = getModifiedSpawnAt xs
        spawnAtMatcher = getModifiedSpawnAt mods
        LocationFilter cannotSpawnMatchers = fold [LocationFilter m | CannotSpawnIn m <- mods]
        (LocationFilter changeSpawnMatchers, changedSpawnMatchers) = fold [(LocationFilter x, y) | ChangeSpawnLocation x y <- mods]
        applyMatcherExclusions ms (SpawnAtFirst sas) =
          SpawnAtFirst (map (applyMatcherExclusions ms) sas)
        applyMatcherExclusions [] m = m
        applyMatcherExclusions (CannotSpawnIn n : xs) (SpawnAt m) =
          applyMatcherExclusions xs (SpawnAt $ m <> NotLocation n)
        applyMatcherExclusions (_ : xs) m = applyMatcherExclusions xs m

      case spawnAtMatcher of
        Nothing -> do
          mlid <- getMaybeLocation iid
          case mlid of
            Just lid -> do
              canSpawn <- lid <!=~> cannotSpawnMatchers
              unchanged <- lid <!=~> changeSpawnMatchers
              if canSpawn && unchanged
                then do
                  windows' <- checkWindows [mkWhen $ Window.EnemyWouldSpawnAt eid lid]
                  pushAll $ windows' : resolve (EnemySpawn (Just iid) lid eid)
                else
                  if not unchanged
                    then do
                      spawnAt enemyId (Just iid)
                        $ applyMatcherExclusions mods
                        $ replaceYouMatcher iid (SpawnAt $ not_ changeSpawnMatchers <> changedSpawnMatchers)
                    else noSpawn a (Just iid)
            Nothing -> noSpawn a (Just iid)
        Just matcher -> spawnAt enemyId (Just iid) (applyMatcherExclusions mods $ replaceYouMatcher iid matcher)
      pure a
    EnemySpawnAtLocationMatching miid locationMatcher eid | eid == enemyId -> do
      activeInvestigatorId <- getActiveInvestigatorId
      lids <- select $ replaceYouMatcher activeInvestigatorId locationMatcher
      case lids of
        [] -> noSpawn a miid
        [lid] -> do
          windows' <- checkWindows [mkWhen $ Window.EnemyWouldSpawnAt eid lid]
          pushAll $ windows' : resolve (EnemySpawn Nothing lid eid)
        xs -> spawnAtOneOf Nothing eid xs
      pure a
    After (InvestigatorEliminated iid) ->
      case enemyPlacement of
        InThreatArea iid' | iid == iid' -> do
          getMaybeLocation iid >>= \case
            Just lid -> do
              push $ EnemyCheckEngagement a.id
              pure $ a & placementL .~ AtLocation lid
            Nothing -> do
              push $ Discard Nothing GameSource (toTarget a)
              pure a
        _ -> pure a
    DisengageEnemy iid eid | eid == enemyId -> case enemyPlacement of
      InThreatArea iid' | iid == iid' -> do
        canDisengage <- iid <=~> InvestigatorCanDisengage
        if canDisengage
          then do
            lid <- getJustLocation iid
            pure $ a & placementL .~ AtLocation lid
          else pure a
      AsSwarm eid' _ -> do
        push $ DisengageEnemy iid eid'
        pure a
      _ -> pure a
    DisengageEnemyFromAll eid | eid == enemyId -> case enemyPlacement of
      InThreatArea iid -> do
        canDisengage <- iid <=~> InvestigatorCanDisengage
        if canDisengage
          then do
            lid <- getJustLocation iid
            pure $ a & placementL .~ AtLocation lid
          else pure a
      AsSwarm eid' _ -> do
        push $ DisengageEnemyFromAll eid'
        pure a
      _ -> pure a
    RemoveAllClues _ target | isTarget a target -> pure $ a & tokensL %~ removeAllTokens Clue
    RemoveAllDoom _ target | isTarget a target -> pure $ a & tokensL %~ removeAllTokens Doom
    RemoveTokens _ target token amount | isTarget a target -> do
      pure $ a & tokensL %~ subtractTokens token amount
    MoveTokens s source _ tType n | isSource a source -> runMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens s _ target tType n | isTarget a target -> runMessage (PlaceTokens s (toTarget a) tType n) a
    PlaceTokens source target token n | isTarget a target -> do
      if token == #doom
        then do
          cannotPlaceDoom <- hasModifier a CannotPlaceDoomOnThis
          if cannotPlaceDoom
            then pure ()
            else do
              batchId <- getRandom
              whenWindow <-
                checkWindows
                  [(mkWhen $ Window.WouldPlaceDoom source target n) {Window.windowBatchId = Just batchId}]

              push $ Would batchId [whenWindow, Do msg]
        else push $ Do msg
      pure a
    Do (PlaceTokens source target token n) | isTarget a target -> do
      if token == #doom
        then do
          cannotPlaceDoom <- hasModifier a CannotPlaceDoomOnThis
          if cannotPlaceDoom
            then pure a
            else do
              when (token == Doom && a.doom == 0) do
                pushM $ checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source target n
              pushAll $ windows [Window.PlacedDoom source (toTarget a) n]
              pure $ a & tokensL %~ addTokens Doom n
        else do
          case token of
            Clue -> pushAll $ windows [Window.PlacedClues source (toTarget a) n]
            Damage -> push $ CheckDefeated source (toTarget a)
            _ -> pure ()
          pure $ a & tokensL %~ addTokens token n
    PlaceKey (isTarget a -> True) k -> do
      pure $ a & keysL %~ insertSet k
    PlaceKey (isTarget a -> False) k -> do
      pure $ a & keysL %~ deleteSet k
    FlipClues target n | isTarget a target -> do
      pure $ a & tokensL %~ flipClues n
    FlipDoom target n | isTarget a target -> do
      pure $ a & tokensL %~ flipDoom n
    ClearTokens target | isTarget a target -> do
      pure $ a & tokensL .~ mempty
    PlaceEnemy eid placement | eid == enemyId -> do
      case placement of
        AtLocation _ -> push $ EnemyCheckEngagement eid
        _ -> pure ()
      checkEntersThreatArea a placement
      pure $ a & placementL .~ placement
    Blanked msg' -> runMessage msg' a
    UseCardAbility iid (isSource a -> True) AbilityAttack _ _ -> do
      sid <- getRandom
      push $ FightEnemy sid iid (toId a) (a.ability AbilityAttack) Nothing #combat False
      pure a
    UseCardAbility iid (isSource a -> True) AbilityEvade _ _ -> do
      sid <- getRandom
      push $ EvadeEnemy sid iid (toId a) (a.ability AbilityEvade) Nothing #agility False
      pure a
    UseCardAbility iid (isSource a -> True) AbilityEngage _ _ -> do
      push $ EngageEnemy iid (toId a) Nothing False
      pure a
    AssignDamage target | isTarget a target -> do
      pushAll $ map (`checkDefeated` a) (keys enemyAssignedDamage)
      pure a
    RemoveAllCopiesOfCardFromGame _ cCode | cCode == toCardCode a -> do
      push $ RemoveEnemy (toId a)
      pure a
    RemoveAllCopiesOfEncounterCardFromGame cardMatcher | toCard a `cardMatch` cardMatcher -> do
      push $ RemoveEnemy (toId a)
      pure a
    SendMessage (isTarget a -> True) msg' -> runMessage msg' a
    RemoveAllAttachments source target -> do
      case placementToAttached a.placement of
        Just attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
      pure a
    PlaceUnderneath (isTarget a -> True) cards -> do
      pure $ a & cardsUnderneathL %~ (nubBy ((==) `on` toCardId) . (<> cards))
    PlaceUnderneath _ cards -> do
      when (toCard a `elem` cards) $ push $ RemoveEnemy (toId a)
      pure a
    ObtainCard c -> do
      pure $ a & cardsUnderneathL %~ filter ((/= c) . toCardId)
    PlaceInBonded _iid card -> do
      when (toCard a == card) do
        removeAllMessagesMatching \case
          Discarded (EnemyTarget aid) _ _ -> aid == a.id
          CheckWindows ws -> flip any ws \case
            (Window.windowType -> Window.Discarded _ _ c) -> toCard a == c
            _ -> False
          Do (CheckWindows ws) -> flip any ws \case
            (Window.windowType -> Window.Discarded _ _ c) -> toCard a == c
            _ -> False
          _ -> False
        push $ RemoveFromGame (toTarget a)
      pure a
    RemoveFromGame target | a `isTarget` target -> do
      a <$ push (RemoveFromPlay $ toSource a)
    RemoveFromPlay source | isSource a source -> do
      windowMsg <-
        checkWindows $ (`Window.mkWindow` Window.LeavePlay (toTarget a)) <$> [#when, #at, #after]
      pushAll [windowMsg, RemovedFromPlay source]
      pure a
    DoBatch _ msg' -> do
      -- generic DoBatch handler
      runMessage (Do msg') a
    ForTarget (isTarget a -> True) msg' -> runMessage msg' a
    _ -> pure a
