{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Enemy.Runner (
  module Arkham.Enemy.Runner,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability as X
import Arkham.Enemy.Helpers as X hiding (EnemyEvade, EnemyFight)
import Arkham.Enemy.Types as X
import Arkham.GameValue as X
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
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator hiding (getModifiedHealth)
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Keyword (_Swarming)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher (
  AssetMatcher (..),
  EnemyMatcher (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  MovesVia (..),
  PreyMatcher (..),
  investigatorAt,
  investigatorEngagedWith,
  locationWithInvestigator,
  preyWith,
  replaceYourLocation,
  pattern InvestigatorCanDisengage,
  pattern MassiveEnemy,
 )
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Phase
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType ()
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Window (mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Data.List.Extra (firstJust)
import Data.Monoid (First (..))

{- | Handle when enemy no longer exists
When an enemy is defeated we need to remove related messages from choices
and if not more choices exist, remove the message entirely
-}
filterOutEnemyMessages :: EnemyId -> Message -> Maybe Message
filterOutEnemyMessages eid (Ask pid q) = case q of
  QuestionLabel {} -> error "currently unhandled"
  Read {} -> error "currently unhandled"
  DropDown {} -> error "currently unhandled"
  PickSupplies {} -> error "currently unhandled"
  ChooseOne msgs -> case mapMaybe (filterOutEnemyUiMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask pid $ ChooseOne x)
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
  ChooseDeck -> Just (Ask pid ChooseDeck)
  choose@ChoosePaymentAmounts {} -> Just (Ask pid choose)
  choose@ChooseAmounts {} -> Just (Ask pid choose)
  PickScenarioSettings -> Just (Ask pid PickScenarioSettings)
  PickCampaignSettings -> Just (Ask pid PickCampaignSettings)
filterOutEnemyMessages eid msg = case msg of
  InitiateEnemyAttack details | eid == attackEnemy details -> Nothing
  EnemyAttack details | eid == attackEnemy details -> Nothing
  Discarded (EnemyTarget eid') _ _ | eid == eid' -> Nothing
  m -> Just m

filterOutEnemyUiMessages :: EnemyId -> UI Message -> Maybe (UI Message)
filterOutEnemyUiMessages eid = \case
  TargetLabel (EnemyTarget eid') _ | eid == eid' -> Nothing
  EvadeLabel eid' _ | eid == eid' -> Nothing
  FightLabel eid' _ | eid == eid' -> Nothing
  other -> Just other

getInvestigatorsAtSameLocation :: HasGame m => EnemyAttrs -> m [InvestigatorId]
getInvestigatorsAtSameLocation attrs = do
  enemyLocation <- field EnemyLocation (toId attrs)
  case enemyLocation of
    Nothing -> pure []
    Just loc -> selectList $ InvestigatorAt $ LocationWithId loc

getPreyMatcher :: HasGame m => EnemyAttrs -> m PreyMatcher
getPreyMatcher a = do
  modifiers' <- getModifiers (toTarget a)
  pure $ foldl' applyModifier (enemyPrey a) modifiers'
 where
  applyModifier _ (ForcePrey p) = p
  applyModifier p _ = p

noSpawn :: HasQueue Message m => EnemyAttrs -> Maybe InvestigatorId -> m ()
noSpawn attrs miid = do
  pushAll
    $ toDiscard GameSource attrs
    : [ Surge iid (toSource attrs)
      | enemySurgeIfUnableToSpawn attrs
      , iid <- toList miid
      ]

isSwarm :: EnemyAttrs -> Bool
isSwarm attrs = case enemyPlacement attrs of
  AsSwarm {} -> True
  _ -> False

instance RunMessage EnemyAttrs where
  runMessage msg a@EnemyAttrs {..} = case msg of
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    EndPhase -> pure $ a & movedFromHunterKeywordL .~ False
    SealedChaosToken token card
      | toCardId card == toCardId a ->
          pure $ a & sealedChaosTokensL %~ (token :)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    RemoveAllChaosTokens face -> pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      prey <- getPreyMatcher a
      preyIds <- selectList prey
      preyIdsWithLocation <-
        forToSnd
          preyIds
          (selectJust . locationWithInvestigator)
      lead <- getLeadPlayer
      for_ (nonEmpty preyIdsWithLocation) $ \iids ->
        push
          $ chooseOrRunOne
            lead
            [ targetLabel
              lid
              [ Will (EnemySpawn (Just iid) lid eid)
              , When (EnemySpawn (Just iid) lid eid)
              , EnemySpawnedAt lid eid
              , EnemyEngageInvestigator eid iid
              , After (EnemySpawn (Just iid) lid eid)
              ]
            | (iid, lid) <- toList iids
            ]
      pure a
    SetBearer (EnemyTarget eid) iid | eid == enemyId -> do
      pure $ a & bearerL ?~ iid
    PlacedSwarmCard eid card | eid == enemyId -> do
      case toCard a of
        EncounterCard ec ->
          pushM $ createEnemyWithPlacement_ (EncounterCard $ ec {ecId = toCardId card}) (AsSwarm eid card)
        PlayerCard pc ->
          pushM $ createEnemyWithPlacement_ (PlayerCard $ pc {pcId = toCardId card}) (AsSwarm eid card)
        VengeanceCard _ -> error "not valid"
      pure a
    EnemySpawn miid lid eid | eid == enemyId -> do
      locations' <- select $ IncludeEmptySpace Anywhere
      keywords <- getModifiedKeywords a
      if lid `notElem` locations'
        then push (toDiscard GameSource eid)
        else do
          let swarms = mapMaybe (preview _Swarming) (toList keywords)

          case swarms of
            [] -> pure ()
            [x] -> do
              n <- getGameValue x
              active <- selectJust ActiveInvestigator
              let swarmInvestigator = fromMaybe active miid
              push $ PlaceSwarmCards swarmInvestigator eid n
            _ -> error "more than one swarming value"

          if Keyword.Aloof `notElem` keywords && Keyword.Massive `notElem` keywords && not enemyExhausted
            then do
              prey <- getPreyMatcher a
              preyIds <- selectList $ preyWith prey $ investigatorAt lid
              investigatorIds <- if null preyIds then selectList $ investigatorAt lid else pure []
              lead <- getLeadPlayer
              let validInvestigatorIds = maybe (preyIds <> investigatorIds) pure miid
              case validInvestigatorIds of
                [] -> push $ EnemyEntered eid lid
                [iid] -> pushAll [EnemyEntered eid lid, EnemyEngageInvestigator eid iid]
                iids ->
                  push
                    $ chooseOne lead
                    $ [targetLabel iid [EnemyEntered eid lid, EnemyEngageInvestigator eid iid] | iid <- iids]
            else
              pushWhen (Keyword.Massive `notElem` keywords)
                $ EnemyEntered eid lid

          when (Keyword.Massive `elem` keywords) do
            investigatorIds <- selectList $ investigatorAt lid
            pushAll $ EnemyEntered eid lid : [EnemyEngageInvestigator eid iid | iid <- investigatorIds]
      pure a
    EnemySpawnedAt lid eid | eid == enemyId -> do
      a <$ push (EnemyEntered eid lid)
    EnemyEntered eid lid | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyEntered eid' lid
          pure a
        _ -> do
          swarm <- selectList $ SwarmOf eid
          pushAll
            =<< traverse
              ( \eid' ->
                  checkWindows
                    ((`mkWindow` Window.EnemyEnters eid' lid) <$> [Timing.When, Timing.After])
              )
              (eid : swarm)
          pure $ a & placementL .~ AtLocation lid
    Ready target | isTarget a target -> do
      modifiers' <- getModifiers (toTarget a)
      phase <- getPhase
      if CannotReady
        `elem` modifiers'
        || (DoesNotReadyDuringUpkeep `elem` modifiers' && phase == UpkeepPhase)
        then pure a
        else do
          lead <- getLeadPlayer
          enemyLocation <- field EnemyLocation enemyId
          iids <-
            fromMaybe []
              <$> traverse
                (selectList . InvestigatorAt . LocationWithId)
                enemyLocation
          keywords <- getModifiedKeywords a

          case enemyPlacement of
            AsSwarm eid' _ -> do
              others <- selectList $ SwarmOf eid' <> NotEnemy (EnemyWithId $ toId a) <> ExhaustedEnemy
              pushAll [Ready (toTarget other) | other <- others]
            _ -> do
              others <- selectList $ SwarmOf (toId a) <> ExhaustedEnemy
              pushAll [Ready (toTarget other) | other <- others]

          unless (null iids) $ do
            unengaged <- selectNone $ investigatorEngagedWith enemyId
            when
              ( Keyword.Aloof
                  `notElem` keywords
                  && (unengaged || Keyword.Massive `elem` keywords)
              )
              $ push
              $ chooseOne
                lead
                [ TargetLabel
                  (InvestigatorTarget iid)
                  [EnemyEngageInvestigator enemyId iid]
                | iid <- iids
                ]
          pure $ a & exhaustedL .~ False
    ReadyExhausted -> do
      modifiers' <- getModifiers (toTarget a)
      -- swarm will be readied by host
      let
        alternativeSources =
          mapMaybe
            ( \case
                AlternativeReady source -> Just source
                _ -> Nothing
            )
            modifiers'
      case alternativeSources of
        [] ->
          when
            (enemyExhausted && DoesNotReadyDuringUpkeep `notElem` modifiers' && not (isSwarm a))
            (pushAll $ resolve (Ready $ toTarget a))
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
                selectList $ AccessibleFrom $ LocationWithId loc
              closestLocationIds <- selectList $ ClosestPathLocation loc lid
              if lid `elem` adjacentLocationIds
                then push $ chooseOne lead [targetLabel lid [EnemyMove enemyId lid]]
                else pushAll [chooseOne lead [targetLabel lid' [EnemyMove enemyId lid'] | lid' <- closestLocationIds]]
      pure a
    MoveUntil lid target | isTarget a target -> do
      case enemyPlacement of
        AsSwarm eid' _ -> push $ MoveUntil lid (EnemyTarget eid')
        _ -> do
          enemyLocation <- field EnemyLocation enemyId
          for_ enemyLocation \loc ->
            when (lid /= loc) $ do
              lead <- getLeadPlayer
              adjacentLocationIds <- selectList $ AccessibleFrom $ LocationWithId loc
              closestLocationIds <- selectList $ ClosestPathLocation loc lid
              if lid `elem` adjacentLocationIds
                then push $ chooseOne lead [targetLabel lid [EnemyMove enemyId lid]]
                else
                  when (notNull closestLocationIds)
                    $ pushAll
                      [ chooseOne
                          lead
                          [ targetLabel lid' [EnemyMove enemyId lid']
                          | lid' <- closestLocationIds
                          ]
                      , MoveUntil lid target
                      ]
      pure a
    EnemyMove eid lid | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyMove eid' lid
          pure a
        _ -> do
          willMove <- canEnterLocation eid lid
          if willMove
            then do
              enemyLocation <- field EnemyLocation enemyId
              leaveWindows <- for enemyLocation
                $ \oldId -> windows [Window.EnemyLeaves eid oldId]
              pushAll
                $ fromMaybe [] leaveWindows
                <> [EnemyEntered eid lid, EnemyCheckEngagement eid]
              pure $ a & placementL .~ AtLocation lid
            else a <$ push (EnemyCheckEngagement eid)
    After (EndTurn _) -> a <$ push (EnemyCheckEngagement $ toId a)
    EnemyCheckEngagement eid | eid == enemyId && not (isSwarm a) -> do
      keywords <- getModifiedKeywords a
      modifiers' <- getModifiers eid
      let
        modifiedFilter iid = do
          if Keyword.Massive `elem` keywords
            then pure True
            else do
              investigatorModifiers <- getModifiers iid
              canEngage <- flip allM investigatorModifiers $ \case
                CannotBeEngagedBy matcher -> notElem eid <$> select matcher
                _ -> pure True
              pure
                $ canEngage
                && EnemyCannotEngage iid
                `notElem` modifiers'
                && CannotBeEngaged
                `notElem` modifiers'
      investigatorIds' <- filterM modifiedFilter =<< getInvestigatorsAtSameLocation a
      prey <- getPreyMatcher a
      preyIds <- selectList $ case prey of
        Prey m ->
          Prey $ m <> AnyInvestigator (map InvestigatorWithId investigatorIds')
        other -> other

      let investigatorIds = if null preyIds then investigatorIds' else preyIds

      lead <- getLeadPlayer
      unengaged <- selectNone $ investigatorEngagedWith enemyId
      when (CannotBeEngaged `elem` modifiers') $ case enemyPlacement of
        InThreatArea iid -> push $ DisengageEnemy iid enemyId
        _ -> pure ()
      when
        ( Keyword.Aloof
            `notElem` keywords
            && (unengaged || Keyword.Massive `elem` keywords)
            && CannotBeEngaged
            `notElem` modifiers'
            && not enemyExhausted
        )
        $ if Keyword.Massive `elem` keywords
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
                $ chooseOne
                  lead
                  [ targetLabel
                    investigatorId
                    [EnemyEngageInvestigator eid investigatorId]
                  | investigatorId <- xs
                  ]
      pure a
    HuntersMove | not enemyExhausted && not (isSwarm a) -> do
      -- TODO: unengaged or not engaged with only prey
      --
      unengaged <- selectNone $ investigatorEngagedWith enemyId
      modifiers' <- getModifiers (EnemyTarget enemyId)
      when (unengaged && CannotMove `notElem` modifiers') $ do
        keywords <- getModifiedKeywords a
        leadInvestigatorId <- getLeadInvestigatorId
        when (Keyword.Hunter `elem` keywords)
          $ pushAll
            [ CheckWindow
                [leadInvestigatorId]
                [mkWindow Timing.When (Window.MovedFromHunter enemyId)]
            , HunterMove (toId a)
            ]
        -- We should never have a case where an enemy has both patrol and
        -- hunter and should only have one patrol keyword
        for_ keywords $ \case
          Keyword.Patrol lMatcher -> push $ PatrolMove (toId a) lMatcher
          _ -> pure ()
      pure a
    HunterMove eid | eid == toId a && not enemyExhausted && not (isSwarm a) -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> do
          modifiers' <- getModifiers (EnemyTarget enemyId)
          let
            locationMatcherModifier =
              if CanEnterEmptySpace `elem` modifiers'
                then IncludeEmptySpace
                else id
            matchForcedTargetLocation = \case
              DuringEnemyPhaseMustMoveToward (LocationTarget lid) -> Just lid
              _ -> Nothing
            forcedTargetLocation =
              firstJust matchForcedTargetLocation modifiers'
          -- applyConnectionMapModifier connectionMap (HunterConnectedTo lid') =
          --   unionWith (<>) connectionMap $ singletonMap loc [lid']
          -- applyConnectionMapModifier connectionMap _ = connectionMap
          -- extraConnectionsMap :: Map LocationId [LocationId] =
          --   foldl' applyConnectionMapModifier mempty modifiers'

          mLocation <- field EnemyLocation eid
          enemiesAsInvestigatorLocations <- case mLocation of
            Nothing -> pure []
            Just lid ->
              selectList
                $ locationMatcherModifier
                $ LocationWithEnemy
                $ NearestEnemyToLocation lid
                $ EnemyWithModifier CountsAsInvestigatorForHunterEnemies

          -- The logic here is an artifact of doing this incorrect
          -- Prey is only used for breaking ties unless we're dealing
          -- with the Only keyword for prey, so here we hardcode prey
          -- to AnyPrey and then find if there are any investigators
          -- who qualify as prey to filter
          prey <- getPreyMatcher a
          matchingClosestLocationIds <- case (forcedTargetLocation, prey) of
            (Just forcedTargetLocationId, _) ->
              -- Lure (1)
              selectList $ locationMatcherModifier $ ClosestPathLocation loc forcedTargetLocationId
            (Nothing, BearerOf _) ->
              selectList
                $ locationMatcherModifier
                $ locationWithInvestigator
                $ fromJustNote
                  "must have bearer"
                  enemyBearer
            (Nothing, RestrictedBearerOf _ _) -> do
              -- this case should never happen, but just in case
              selectList
                $ locationMatcherModifier
                $ locationWithInvestigator
                $ fromJustNote
                  "must have bearer"
                  enemyBearer
            (Nothing, OnlyPrey onlyPrey) ->
              selectList
                $ locationMatcherModifier
                $ LocationWithInvestigator
                $ onlyPrey
                <> NearestToEnemy
                  (EnemyWithId eid)
            (Nothing, _prey) -> do
              investigatorLocations <-
                selectList
                  $ locationMatcherModifier
                  $ LocationWithInvestigator
                  $ NearestToEnemy
                  $ EnemyWithId eid
              case mLocation of
                Nothing -> pure investigatorLocations
                Just lid ->
                  selectList
                    $ locationMatcherModifier
                    $ NearestLocationToLocation
                      lid
                      (LocationMatchAny $ map LocationWithId (enemiesAsInvestigatorLocations <> investigatorLocations))

          preyIds <- select prey
          let includeEnemies = prey == Prey Anyone

          filteredClosestLocationIds <-
            flip filterM matchingClosestLocationIds $ \lid -> do
              hasInvestigators <-
                notNull
                  . intersect preyIds
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

          (leadInvestigatorId, lead) <- getLeadInvestigatorPlayer
          pathIds <-
            concat
              <$> traverse
                (selectList . locationMatcherModifier . ClosestPathLocation loc)
                destinationLocationIds
          case pathIds of
            [] -> pure a
            [lid] -> do
              pushAll
                [ EnemyMove enemyId lid
                , CheckWindow
                    [leadInvestigatorId]
                    [mkWindow Timing.After (Window.MovedFromHunter enemyId)]
                , CheckWindow
                    [leadInvestigatorId]
                    [mkWindow Timing.After (Window.EnemyMovesTo lid MovedViaHunter enemyId)]
                ]
              pure $ a & movedFromHunterKeywordL .~ True
            ls -> do
              push
                $ chooseOrRunOne
                  lead
                  [ targetLabel
                    l
                    [ EnemyMove enemyId l
                    , CheckWindow
                        [leadInvestigatorId]
                        [mkWindow Timing.After (Window.MovedFromHunter enemyId)]
                    , CheckWindow
                        [leadInvestigatorId]
                        [mkWindow Timing.After (Window.EnemyMovesTo l MovedViaHunter enemyId)]
                    ]
                  | l <- ls
                  ]
              pure $ a & movedFromHunterKeywordL .~ True
    PatrolMove eid lMatcher | eid == toId a && not enemyExhausted && not (isSwarm a) -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> do
          modifiers' <- getModifiers (EnemyTarget enemyId)
          let
            locationMatcherModifier =
              if CanEnterEmptySpace `elem` modifiers'
                then IncludeEmptySpace
                else id

          destinationLocationIds <-
            selectList
              $ NearestLocationToLocation loc (locationMatcherModifier lMatcher)

          (leadInvestigatorId, lead) <- getLeadInvestigatorPlayer
          pathIds <-
            concat
              <$> traverse
                (selectList . locationMatcherModifier . ClosestPathLocation loc)
                destinationLocationIds
          case pathIds of
            [] -> pure ()
            [lid] -> do
              pushAll
                [ EnemyMove enemyId lid
                , -- , CheckWindow
                  --     [leadInvestigatorId]
                  --     [mkWindow Timing.After (Window.MovedFromHunter enemyId)]
                  CheckWindow
                    [leadInvestigatorId]
                    [mkWindow Timing.After (Window.EnemyMovesTo lid MovedViaOther enemyId)]
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
                      CheckWindow
                        [leadInvestigatorId]
                        [mkWindow Timing.After (Window.EnemyMovesTo l MovedViaOther enemyId)]
                    ]
                  | l <- ls
                  ]
          pure a
    EnemiesAttack | not enemyExhausted -> do
      modifiers' <- getModifiers (EnemyTarget enemyId)
      unless (CannotAttack `elem` modifiers') $ do
        iids <- selectList $ investigatorEngagedWith enemyId
        pushAll
          $ map
            ( \iid ->
                EnemyWillAttack
                  $ (enemyAttack enemyId a iid)
                    { attackDamageStrategy = enemyDamageStrategy
                    , attackExhaustsEnemy = True
                    }
            )
            iids
      pure a
    AttackEnemy iid eid source mTarget skillType | eid == enemyId -> do
      enemyFight' <- modifiedEnemyFight iid a
      push
        $ fight
          iid
          source
          (maybe (EnemyTarget eid) (ProxyTarget (EnemyTarget eid)) mTarget)
          skillType
          enemyFight'
      pure a
    PassedSkillTest iid (Just Action.Fight) source (SkillTestInitiatorTarget target) _ n
      | isActionTarget a target -> do
          whenWindow <-
            checkWindows
              [mkWindow Timing.When (Window.SuccessfulAttackEnemy iid enemyId n)]
          afterSuccessfulWindow <-
            checkWindows
              [mkWindow Timing.After (Window.SuccessfulAttackEnemy iid enemyId n)]
          afterWindow <-
            checkWindows
              [mkWindow Timing.After (Window.EnemyAttacked iid source enemyId)]
          pushAll
            [ whenWindow
            , Successful
                (Action.Fight, toProxyTarget target)
                iid
                source
                (toActionTarget target)
                n
            , afterSuccessfulWindow
            , afterWindow
            ]

          pure a
    Successful (Action.Fight, _) iid source target _ | isTarget a target -> do
      a <$ push (InvestigatorDamageEnemy iid enemyId source)
    FailedSkillTest iid (Just Action.Fight) source (SkillTestInitiatorTarget target) _ n | isTarget a target -> do
      keywords <- getModifiedKeywords a
      modifiers' <- getModifiers iid
      pushAll
        $ [ FailedAttackEnemy iid enemyId
          , CheckWindow
              [iid]
              [mkWindow Timing.After (Window.FailAttackEnemy iid enemyId n)]
          , CheckWindow
              [iid]
              [mkWindow Timing.After (Window.EnemyAttacked iid source enemyId)]
          ]
        <> [ EnemyAttack
            $ (enemyAttack enemyId a iid)
              { attackDamageStrategy = enemyDamageStrategy
              }
           | Keyword.Retaliate
              `elem` keywords
           , IgnoreRetaliate
              `notElem` modifiers'
           , not enemyExhausted
              || CanRetaliateWhileExhausted
              `elem` modifiers'
           ]
      pure a
    EnemyAttackIfEngaged eid miid | eid == enemyId -> do
      case miid of
        Just iid -> do
          shouldAttack <- member iid <$> select (investigatorEngagedWith eid)
          when shouldAttack
            $ push
            $ EnemyAttack
            $ (enemyAttack enemyId a iid)
              { attackDamageStrategy = enemyDamageStrategy
              }
        Nothing -> do
          iids <- selectList $ investigatorEngagedWith eid
          pushAll
            [ EnemyAttack
              $ (enemyAttack enemyId a iid)
                { attackDamageStrategy = enemyDamageStrategy
                }
            | iid <- iids
            ]
      pure a
    EnemyEvaded iid eid | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyEvaded iid eid'
          pure a
        _ -> do
          modifiers <- getModifiers (InvestigatorTarget iid)
          lid <- fieldJust EnemyLocation eid
          let
            updatePlacement =
              if DoNotDisengageEvaded `elem` modifiers
                then id
                else placementL .~ AtLocation lid
            updateExhausted =
              if DoNotExhaustEvaded `elem` modifiers
                then id
                else exhaustedL .~ True
          others <- selectList $ SwarmOf (toId a) <> ReadyEnemy
          pushAll [Exhaust (toTarget other) | other <- others]
          pure $ a & updatePlacement & updateExhausted
    Exhaust (isTarget a -> True) -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          others <- selectList $ SwarmOf eid' <> NotEnemy (EnemyWithId $ toId a) <> ReadyEnemy
          pushAll [Exhaust (toTarget other) | other <- others]
        _ -> do
          others <- selectList $ SwarmOf (toId a) <> ReadyEnemy
          pushAll [Exhaust (toTarget other) | other <- others]
      pure $ a & exhaustedL .~ True
    TryEvadeEnemy iid eid source mTarget skillType | eid == enemyId -> do
      mEnemyEvade' <- modifiedEnemyEvade a
      case mEnemyEvade' of
        Just n ->
          push
            $ evade
              iid
              source
              (maybe (EnemyTarget eid) (ProxyTarget (EnemyTarget eid)) mTarget)
              skillType
              n
        Nothing -> error "No evade value"
      pure a
    PassedSkillTest iid (Just Action.Evade) source (SkillTestInitiatorTarget target) _ n
      | isActionTarget a target ->
          do
            whenWindow <-
              checkWindows
                [mkWindow Timing.When (Window.SuccessfulEvadeEnemy iid enemyId n)]
            afterWindow <-
              checkWindows
                [mkWindow Timing.After (Window.SuccessfulEvadeEnemy iid enemyId n)]
            a
              <$ pushAll
                [ whenWindow
                , Successful
                    (Action.Evade, toProxyTarget target)
                    iid
                    source
                    (toActionTarget target)
                    n
                , afterWindow
                ]
    Successful (Action.Evade, _) iid _ target _ | isTarget a target -> do
      a <$ push (EnemyEvaded iid enemyId)
    FailedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _ n
      | isTarget a target ->
          do
            keywords <- getModifiedKeywords a
            whenWindow <-
              checkWindows
                [mkWindow Timing.When (Window.FailEvadeEnemy iid enemyId n)]
            afterWindow <-
              checkWindows
                [mkWindow Timing.After (Window.FailEvadeEnemy iid enemyId n)]
            pushAll
              $ [whenWindow, afterWindow]
              <> [ EnemyAttack
                  $ (enemyAttack enemyId a iid)
                    { attackDamageStrategy = enemyDamageStrategy
                    }
                 | Keyword.Alert `elem` keywords
                 ]
            pure a
    InitiateEnemyAttack details | attackEnemy details == enemyId -> do
      push $ EnemyAttack details
      pure a
    EnemyAttack details | attackEnemy details == enemyId -> do
      whenAttacksWindow <-
        checkWindows
          [mkWindow Timing.When (Window.EnemyAttacks details)]
      afterAttacksEventIfCancelledWindow <-
        checkWindows
          [mkWindow Timing.After (Window.EnemyAttacksEvenIfCancelled details)]
      whenWouldAttackWindow <-
        checkWindows
          [mkWindow Timing.When (Window.EnemyWouldAttack details)]
      pushAll
        [ whenWouldAttackWindow
        , whenAttacksWindow
        , PerformEnemyAttack details
        , After (PerformEnemyAttack details)
        , afterAttacksEventIfCancelledWindow
        ]
      pure a
    PerformEnemyAttack details | attackEnemy details == enemyId -> do
      modifiers <- getModifiers (attackTarget details)
      sourceModifiers <- getModifiers (sourceToTarget $ attackSource details)

      let
        applyModifiers cards (CancelAttacksByEnemies c n) = do
          canceled <- member enemyId <$> select n
          pure
            $ if canceled
              then c : cards
              else cards
        applyModifiers m _ = pure m

      cardsThatCanceled <- foldM applyModifiers [] modifiers

      ignoreWindows <- for cardsThatCanceled $ \card ->
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ CardSource card)]

      let
        allowAttack =
          and
            [ null cardsThatCanceled
            , EffectsCannotBeCanceled `notElem` sourceModifiers && attackCanBeCanceled details
            ]

      case attackTarget details of
        InvestigatorTarget iid ->
          pushAll
            $ [ InvestigatorAssignDamage
                iid
                (EnemyAttackSource enemyId)
                (attackDamageStrategy details)
                enemyHealthDamage
                enemySanityDamage
              | allowAttack
              ]
            <> [Exhaust (toTarget a) | allowAttack, attackExhaustsEnemy details]
            <> ignoreWindows
            <> [After (EnemyAttack details)]
        _ -> error "Unhandled"
      pure a
    After (EnemyAttack details) | attackEnemy details == toId a -> do
      afterAttacksWindow <-
        checkWindows
          [mkWindow Timing.After (Window.EnemyAttacks details)]
      push afterAttacksWindow
      pure a
    HealDamage (EnemyTarget eid) source n | eid == enemyId -> do
      afterWindow <-
        checkWindows
          [mkWindow Timing.After (Window.Healed DamageType (toTarget a) source n)]
      push afterWindow
      pure $ a & tokensL %~ subtractTokens Token.Damage n
    HealAllDamage (EnemyTarget eid) source | eid == enemyId -> do
      afterWindow <-
        checkWindows
          [ mkWindow
              Timing.After
              (Window.Healed DamageType (toTarget a) source (enemyDamage a))
          ]
      push afterWindow
      pure $ a & tokensL %~ removeAllTokens Token.Damage
    Msg.EnemyDamage eid damageAssignment | eid == enemyId -> do
      let
        source = damageAssignmentSource damageAssignment
        damageEffect = damageAssignmentDamageEffect damageAssignment
        damageAmount = damageAssignmentAmount damageAssignment
      canDamage <- sourceCanDamageEnemy eid source
      when
        canDamage
        do
          dealtDamageWhenMsg <-
            checkWindows
              [ mkWindow
                  Timing.When
                  ( Window.DealtDamage
                      source
                      damageEffect
                      (toTarget a)
                      damageAmount
                  )
              ]
          dealtDamageAfterMsg <-
            checkWindows
              [ mkWindow
                  Timing.After
                  ( Window.DealtDamage
                      source
                      damageEffect
                      (toTarget a)
                      damageAmount
                  )
              ]
          takeDamageWhenMsg <-
            checkWindows
              [ mkWindow
                  Timing.When
                  (Window.TakeDamage source damageEffect (toTarget a) damageAmount)
              ]
          takeDamageAfterMsg <-
            checkWindows
              [ mkWindow
                  Timing.After
                  (Window.TakeDamage source damageEffect (toTarget a) damageAmount)
              ]
          pushAll
            [ dealtDamageWhenMsg
            , dealtDamageAfterMsg
            , takeDamageWhenMsg
            , EnemyDamaged eid damageAssignment
            , takeDamageAfterMsg
            ]
      pure a
    EnemyDamaged eid damageAssignment | eid == enemyId -> do
      let
        direct = damageAssignmentDirect damageAssignment
        source = damageAssignmentSource damageAssignment
        amount = damageAssignmentAmount damageAssignment
      canDamage <- sourceCanDamageEnemy eid source
      if canDamage
        then do
          amount' <- getModifiedDamageAmount a direct amount
          let
            damageAssignment' =
              damageAssignment {damageAssignmentAmount = amount'}
            combine l r =
              if damageAssignmentDamageEffect l
                == damageAssignmentDamageEffect r
                then
                  l
                    { damageAssignmentAmount =
                        damageAssignmentAmount l
                          + damageAssignmentAmount r
                    }
                else
                  error
                    $ "mismatched damage assignments\n\nassignment: "
                    <> show l
                    <> "\nnew assignment: "
                    <> show r
          unless (damageAssignmentDelayed damageAssignment')
            $ push
            $ checkDefeated source eid
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
            modifiedHealth <- getModifiedHealth a
            when (enemyDamage a >= modifiedHealth) $ do
              whenMsg <- checkWindows [mkWhen $ Window.EnemyWouldBeDefeated eid]
              afterMsg <- checkWindows [mkWhen $ Window.EnemyWouldBeDefeated eid]
              pushAll
                $ [ whenMsg
                  , afterMsg
                  , EnemyDefeated
                      eid
                      (toCardId a)
                      source
                      (setToList $ toTraits a)
                  ]
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
            mOnlyBeDefeatedByModifier =
              getFirst $ foldMap canOnlyBeDefeatedByModifier modifiers'
          validDefeat <-
            ( ( canBeDefeated
                  && not hasSwarm
              )
                &&
              )
              <$> maybe (pure True) (sourceMatches source) mOnlyBeDefeatedByModifier
          when validDefeat $ do
            modifiedHealth <- getModifiedHealth a
            when (enemyDamage a + amount' >= modifiedHealth) $ do
              let excess = (enemyDamage a + amount') - modifiedHealth
              let
                mSwarmOf = case enemyPlacement of
                  AsSwarm eid' _ -> Just eid'
                  _ -> Nothing
              controller <- maybe getLeadPlayer getPlayer =<< getSourceController source

              excessDamageTargets <- case mSwarmOf of
                Nothing -> pure []
                Just eid' -> (eid' :) <$> selectList (NotEnemy (EnemyWithId $ toId a) <> SwarmOf eid')

              whenMsg <-
                checkWindows
                  [mkWindow Timing.When (Window.EnemyWouldBeDefeated eid)]
              afterMsg <-
                checkWindows
                  [mkWindow Timing.After (Window.EnemyWouldBeDefeated eid)]
              whenExcessMsg <-
                checkWindows
                  [ mkWindow
                    Timing.When
                    ( Window.DealtExcessDamage
                        source
                        damageEffect
                        (EnemyTarget eid)
                        excess
                    )
                  | excess > 0
                  ]
              afterExcessMsg <-
                checkWindows
                  [ mkWindow
                    Timing.After
                    ( Window.DealtExcessDamage
                        source
                        damageEffect
                        (EnemyTarget eid)
                        excess
                    )
                  | excess > 0
                  ]
              pushAll
                $ [ whenExcessMsg
                  , afterExcessMsg
                  , whenMsg
                  , afterMsg
                  , EnemyDefeated
                      eid
                      (toCardId a)
                      source
                      (setToList $ toTraits a)
                  ]
                <> ( guard (notNull excessDamageTargets && excess > 0)
                      *> [ chooseOne
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
                   )
          pure $ a & tokensL %~ addTokens Token.Damage amount' & assignedDamageL .~ mempty
    DefeatEnemy eid _ source | eid == enemyId -> do
      canBeDefeated <- withoutModifier a CannotBeDefeated
      modifiedHealth <- getModifiedHealth a
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
      when validDefeat
        $ push
        $ EnemyDefeated
          eid
          (toCardId a)
          source
          (setToList $ toTraits a)
      pure a
    EnemyDefeated eid _ source _ | eid == toId a -> do
      modifiedHealth <- getModifiedHealth a
      let
        defeatedByDamage = enemyDamage a >= modifiedHealth
        defeatedBy = if defeatedByDamage then DefeatedByDamage source else DefeatedByOther source
      miid <- getSourceController source
      whenMsg <-
        checkWindows
          [mkWindow Timing.When (Window.EnemyDefeated miid defeatedBy eid)]
      afterMsg <-
        checkWindows
          [mkWindow Timing.After (Window.EnemyDefeated miid defeatedBy eid)]
      victory <- getVictoryPoints eid
      vengeance <- getVengeancePoints eid
      let
        victoryMsgs =
          [DefeatedAddToVictory $ toTarget a | isJust (victory <|> vengeance)]
        defeatMsgs =
          if isJust (victory <|> vengeance)
            then resolve $ RemoveEnemy eid
            else [Discard miid GameSource $ toTarget a]

      withQueue_ $ mapMaybe (filterOutEnemyMessages eid)

      pushAll
        $ [whenMsg, When msg, After msg]
        <> victoryMsgs
        <> [afterMsg]
        <> defeatMsgs
      pure $ a & keysL .~ mempty
    Discard _ source target | a `isTarget` target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      let
        card = case enemyPlacement of
          AsSwarm _ c -> c
          _ -> toCard a
      pushAll
        $ windows'
        <> [ RemovedFromPlay $ toSource a
           , Discarded (toTarget a) source card
           , Do (Discarded (toTarget a) source card)
           ]
      pure $ a & keysL .~ mempty
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
      enemyAssets <- selectList $ EnemyAsset enemyId
      windowMsg <-
        checkWindows
          $ (`mkWindow` Window.LeavePlay (toTarget a))
          <$> [Timing.When, Timing.After]
      pushAll
        $ windowMsg
        : map (toDiscard GameSource) enemyAssets
          <> [UnsealChaosToken token | token <- enemySealedChaosTokens]
      pure a
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyEngageInvestigator eid' iid
          pure a
        _ -> do
          lid <- getJustLocation iid
          enemyLocation <- field EnemyLocation eid
          when (Just lid /= enemyLocation) $ push $ EnemyEntered eid lid
          massive <- eid <=~> MassiveEnemy
          pure $ a & (if massive then id else placementL .~ InThreatArea iid)
    EngageEnemy iid eid mTarget False | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EngageEnemy iid eid' mTarget False
          pure a
        _ -> do
          massive <- eid <=~> MassiveEnemy
          pure $ a & (if massive then id else placementL .~ InThreatArea iid)
    WhenWillEnterLocation iid lid -> do
      case enemyPlacement of
        InThreatArea iid' | iid' == iid -> do
          keywords <- getModifiedKeywords a
          willMove <- canEnterLocation enemyId lid
          -- TODO: we may not need to check massive anymore since we look at placement
          push
            $ if Keyword.Massive `notElem` keywords && willMove
              then EnemyEntered enemyId lid
              else DisengageEnemy iid enemyId
        _ -> pure ()
      pure a
    CheckAttackOfOpportunity iid isFast | not isFast && not enemyExhausted -> do
      willAttack <- member iid <$> select (investigatorEngagedWith enemyId)
      when willAttack $ do
        modifiers' <- getModifiers enemyId
        unless (CannotMakeAttacksOfOpportunity `elem` modifiers')
          $ push
          $ EnemyWillAttack
          $ EnemyAttackDetails
            { attackEnemy = enemyId
            , attackTarget = InvestigatorTarget iid
            , attackDamageStrategy = enemyDamageStrategy
            , attackType = AttackOfOpportunity
            , attackExhaustsEnemy = False
            , attackSource = toSource a
            , attackCanBeCanceled = True
            }
      pure a
    InvestigatorDrawEnemy iid eid | eid == enemyId -> do
      modifiers' <- getModifiers enemyId
      let
        getModifiedSpawnAt [] = enemySpawnAt
        getModifiedSpawnAt (ForceSpawnLocation m : _) = Just $ SpawnAt m
        getModifiedSpawnAt (_ : xs) = getModifiedSpawnAt xs
        spawnAtMatcher = getModifiedSpawnAt modifiers'
      case spawnAtMatcher of
        Nothing -> do
          mlid <- getMaybeLocation iid
          case mlid of
            Just lid -> do
              windows' <- checkWindows [mkWindow Timing.When (Window.EnemyWouldSpawnAt eid lid)]
              pushAll $ windows' : resolve (EnemySpawn (Just iid) lid eid)
            Nothing -> noSpawn a (Just iid)
        Just matcher -> do
          let
            applyMatcherExclusions ms (SpawnAtFirst sas) =
              SpawnAtFirst (map (applyMatcherExclusions ms) sas)
            applyMatcherExclusions [] m = m
            applyMatcherExclusions (CannotSpawnIn n : xs) (SpawnAt m) =
              applyMatcherExclusions xs (SpawnAt $ m <> NotLocation n)
            applyMatcherExclusions (_ : xs) m = applyMatcherExclusions xs m
          spawnAt enemyId (applyMatcherExclusions modifiers' matcher)
      pure a
    EnemySpawnAtLocationMatching miid locationMatcher eid | eid == enemyId -> do
      activeInvestigatorId <- getActiveInvestigatorId
      lids <- selectList $ replaceYourLocation activeInvestigatorId locationMatcher
      leadInvestigatorId <- getLeadInvestigatorId
      case lids of
        [] -> noSpawn a miid
        [lid] -> do
          windows' <-
            checkWindows
              [mkWindow Timing.When (Window.EnemyWouldSpawnAt eid lid)]
          pushAll $ windows' : resolve (EnemySpawn miid lid eid)
        xs -> spawnAtOneOf (fromMaybe leadInvestigatorId miid) eid xs
      pure a
    InvestigatorEliminated iid -> case enemyPlacement of
      InThreatArea iid' | iid == iid' -> do
        lid <- getJustLocation iid
        pure $ a & placementL .~ AtLocation lid
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
    PlaceTokens source target Doom amount | isTarget a target -> do
      modifiers' <- getModifiers (toTarget a)
      if CannotPlaceDoomOnThis `elem` modifiers'
        then pure a
        else do
          windows' <- windows [Window.PlacedDoom source (toTarget a) amount]
          pushAll windows'
          pure $ a & tokensL %~ addTokens Doom amount
    PlaceTokens source target token n | isTarget a target -> do
      case token of
        Clue -> do
          windows' <- windows [Window.PlacedClues source (toTarget a) n]
          pushAll windows'
        _ -> pure ()
      pure $ a & tokensL %~ addTokens token n
    PlaceKey (isTarget a -> True) k -> do
      pure $ a & keysL %~ insertSet k
    PlaceKey (isTarget a -> False) k -> do
      pure $ a & keysL %~ deleteSet k
    RemoveClues _ target n | isTarget a target -> do
      pure $ a & tokensL %~ subtractTokens Clue n
    MovedClues _ (isTarget a -> True) n -> do
      pure $ a & tokensL %~ addTokens #clue n
    MovedClues (isSource a -> True) _ n -> do
      pure $ a & tokensL %~ subtractTokens #clue n
    FlipClues target n | isTarget a target -> do
      pure $ a & tokensL %~ flipClues n
    PlaceEnemyInVoid eid | eid == enemyId -> do
      withQueue_ $ mapMaybe (filterOutEnemyMessages eid)
      pure
        $ a
        & (placementL .~ OutOfPlay VoidZone)
        & (exhaustedL .~ False)
        & (tokensL %~ removeAllTokens Doom . removeAllTokens Clue . removeAllTokens Token.Damage)
    PlaceEnemy eid placement | eid == enemyId -> do
      push $ EnemyCheckEngagement eid
      pure $ a & placementL .~ placement
    Blanked msg' -> runMessage msg' a
    UseCardAbility iid (isSource a -> True) AbilityAttack _ _ -> do
      push $ FightEnemy iid (toId a) (toSource iid) Nothing #combat False
      pure a
    UseCardAbility iid (isSource a -> True) AbilityEvade _ _ -> do
      push $ EvadeEnemy iid (toId a) (toSource iid) Nothing #agility False
      pure a
    UseCardAbility iid (isSource a -> True) AbilityEngage _ _ -> do
      push $ EngageEnemy iid (toId a) Nothing False
      pure a
    AssignDamage target | isTarget a target -> do
      let sources = keys enemyAssignedDamage
      pushAll $ map (`checkDefeated` a) sources
      pure a
    Msg.Damage (isTarget a -> True) _ _ -> do
      error $ "Use EnemyDamage instead"
    RemoveAllCopiesOfCardFromGame _ cCode | cCode == toCardCode a -> do
      push $ RemoveEnemy (toId a)
      pure a
    SendMessage (isTarget a -> True) msg' -> do
      runMessage msg' a
    _ -> pure a
