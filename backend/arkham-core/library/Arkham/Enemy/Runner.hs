{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Enemy.Runner (module Arkham.Enemy.Runner, module X) where

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
import Arkham.Id as X (AsId (..))
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
  investigatorAt,
  investigatorEngagedWith,
  locationWithInvestigator,
  oneOf,
  preyWith,
  replaceYouMatcher,
  pattern InvestigatorCanDisengage,
  pattern MassiveEnemy,
 )
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType ()
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Data.List qualified as List
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
    Just loc -> select $ investigatorAt loc

getPreyMatcher :: HasGame m => EnemyAttrs -> m PreyMatcher
getPreyMatcher a = do
  mods <- getModifiers a
  pure $ foldl' applyModifier (enemyPrey a) mods
 where
  applyModifier _ (ForcePrey p) = p
  applyModifier p _ = p

noSpawn :: HasQueue Message m => EnemyAttrs -> Maybe InvestigatorId -> m ()
noSpawn attrs miid = do
  let noSpawnMsg = case enemyUnableToSpawn attrs of
        DiscardIfUnableToSpawn -> toDiscard GameSource (toId attrs)
        ShuffleBackInIfUnableToSpawn -> ShuffleBackIntoEncounterDeck (toTarget attrs)
  pushAll $ noSpawnMsg
    : [ Surge iid (toSource attrs) | enemySurgeIfUnableToSpawn attrs, iid <- toList miid
      ]

isSwarm :: EnemyAttrs -> Bool
isSwarm attrs = case enemyPlacement attrs of
  AsSwarm {} -> True
  _ -> False

instance RunMessage EnemyAttrs where
  runMessage msg a@EnemyAttrs {..} = case msg of
    UpdateEnemy eid upd | eid == enemyId -> do
      -- TODO: we may want life cycles around this, generally this might just be a bad idea
      pure $ updateEnemy [upd] a
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    EndPhase -> pure $ a & movedFromHunterKeywordL .~ False
    SealedChaosToken token card | toCardId card == toCardId a -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    RemoveAllChaosTokens face -> pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      prey <- getPreyMatcher a
      preyIds <- select prey
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
              preyIds <- select $ preyWith prey $ investigatorAt lid
              investigatorIds <- if null preyIds then select $ investigatorAt lid else pure []
              lead <- getLeadPlayer
              let allIds = preyIds <> investigatorIds
              let validInvestigatorIds = maybe (preyIds <> investigatorIds) (\iid -> guard (iid `elem` allIds) $> iid) miid
              case validInvestigatorIds of
                [] -> push $ EnemyEntered eid lid
                [iid] -> pushAll $ EnemyEntered eid lid : [EnemyEngageInvestigator eid iid]
                iids ->
                  push
                    $ chooseOne lead
                    $ [targetLabel iid [EnemyEntered eid lid, EnemyEngageInvestigator eid iid] | iid <- iids]
            else pushWhen (#massive `notElem` keywords) $ EnemyEntered eid lid

          when (#massive `elem` keywords) do
            investigatorIds <- select $ investigatorAt lid
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
          swarm <- select $ SwarmOf eid
          pushAll
            =<< traverse
              (\eid' -> checkWindows (($ Window.EnemyEnters eid' lid) <$> [mkWhen, mkAfter]))
              (eid : swarm)
          pure $ a & placementL .~ AtLocation lid
    Ready (isTarget a -> True) -> do
      mods <- getModifiers a
      phase <- getPhase
      if CannotReady `elem` mods || (DoesNotReadyDuringUpkeep `elem` mods && phase == #upkeep)
        then pure ()
        else do
          wouldDo msg (Window.WouldReady $ toTarget a) (Window.Readies $ toTarget a)
      pure a
    Do (Ready (isTarget a -> True)) -> do
      mods <- getModifiers a
      phase <- getPhase
      if CannotReady `elem` mods || (DoesNotReadyDuringUpkeep `elem` mods && phase == #upkeep)
        then pure a
        else do
          lead <- getLeadPlayer
          enemyLocation <- field EnemyLocation enemyId
          iids <- fromMaybe [] <$> traverse (select . investigatorAt) enemyLocation
          keywords <- getModifiedKeywords a

          case enemyPlacement of
            AsSwarm eid' _ -> do
              others <- select $ SwarmOf eid' <> not_ (EnemyWithId $ toId a) <> ExhaustedEnemy
              pushAll $ map (Ready . toTarget) others
            _ -> do
              others <- select $ SwarmOf (toId a) <> ExhaustedEnemy
              pushAll $ map (Ready . toTarget) others

          unless (null iids) $ do
            unengaged <- selectNone $ investigatorEngagedWith enemyId
            when (all (`notElem` keywords) [#aloof, #massive] && unengaged) $ do
              push $ chooseOne lead $ targetLabels iids (only . EnemyEngageInvestigator enemyId)
          pure $ a & exhaustedL .~ False
    ReadyExhausted -> do
      mods <- getModifiers a
      -- swarm will be readied by host
      let alternativeSources = [source | AlternativeReady source <- mods]
      case alternativeSources of
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
        ToLocation destinationLocationId -> do
          push $ EnemyMove (toId a) destinationLocationId
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
            leaveWindows <- for enemyLocation \oldId -> windows [Window.EnemyLeaves eid oldId]
            pushAll $ fromMaybe [] leaveWindows <> [EnemyEntered eid lid, EnemyCheckEngagement eid]
            pure $ a & placementL .~ AtLocation lid
          else a <$ push (EnemyCheckEngagement eid)
    After (EndTurn _) -> a <$ push (EnemyCheckEngagement $ toId a)
    EnemyCheckEngagement eid | eid == enemyId && not (isSwarm a) -> do
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
    HuntersMove | not enemyExhausted && not (isSwarm a) -> do
      -- TODO: unengaged or not engaged with only prey
      --
      unengaged <- selectNone $ investigatorEngagedWith enemyId
      mods <- getModifiers enemyId
      when (unengaged && CannotMove `notElem` mods) $ do
        keywords <- getModifiedKeywords a
        leadId <- getLeadInvestigatorId
        when (Keyword.Hunter `elem` keywords) do
          pushAll
            [ CheckWindow [leadId] [mkWhen $ Window.MovedFromHunter enemyId]
            , HunterMove (toId a)
            ]
        -- We should never have a case where an enemy has both patrol and
        -- hunter and should only have one patrol keyword
        for_ keywords \case
          Keyword.Patrol lMatcher -> push $ PatrolMove (toId a) lMatcher
          _ -> pure ()
      pure a
    HunterMove eid | eid == toId a && not enemyExhausted && not (isSwarm a) -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> do
          mods <- getModifiers enemyId
          let
            locationMatcherModifier =
              if CanEnterEmptySpace `elem` mods
                then IncludeEmptySpace
                else id
            matchForcedTargetLocation = \case
              DuringEnemyPhaseMustMoveToward (LocationTarget lid) -> Just lid
              _ -> Nothing
            forcedTargetLocation = firstJust matchForcedTargetLocation mods
          -- applyConnectionMapModifier connectionMap (HunterConnectedTo lid') =
          --   unionWith (<>) connectionMap $ singletonMap loc [lid']
          -- applyConnectionMapModifier connectionMap _ = connectionMap
          -- extraConnectionsMap :: Map LocationId [LocationId] =
          --   foldl' applyConnectionMapModifier mempty modifiers'

          enemiesAsInvestigatorLocations <-
            select
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
          matchingClosestLocationIds <- case (forcedTargetLocation, prey) of
            (Just forcedTargetLocationId, _) ->
              -- Lure (1)
              select $ locationMatcherModifier $ ClosestPathLocation loc forcedTargetLocationId
            (Nothing, BearerOf _) ->
              select
                $ locationMatcherModifier
                $ locationWithInvestigator
                $ fromJustNote
                  "must have bearer"
                  enemyBearer
            (Nothing, RestrictedBearerOf _ _) -> do
              -- this case should never happen, but just in case
              select
                $ locationMatcherModifier
                $ locationWithInvestigator
                $ fromJustNote
                  "must have bearer"
                  enemyBearer
            (Nothing, OnlyPrey onlyPrey) ->
              select
                $ locationMatcherModifier
                $ LocationWithInvestigator
                $ onlyPrey
                <> NearestToEnemy
                  (EnemyWithId eid)
            (Nothing, _prey) -> do
              investigatorLocations <-
                select
                  $ locationMatcherModifier
                  $ LocationWithInvestigator
                  $ NearestToEnemy (EnemyWithId eid)
                  <> CanBeHuntedBy eid
              select
                $ locationMatcherModifier
                $ NearestLocationToLocation
                  loc
                  (LocationMatchAny $ map LocationWithId (enemiesAsInvestigatorLocations <> investigatorLocations))

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

          (leadId, lead) <- getLeadInvestigatorPlayer
          pathIds <-
            concatForM
              destinationLocationIds
              (select . (LocationCanBeEnteredBy enemyId <>) . locationMatcherModifier . ClosestPathLocation loc)
          case pathIds of
            [] -> pure a
            [lid] -> do
              pushAll
                [ EnemyMove enemyId lid
                , CheckWindow [leadId] [mkAfter $ Window.MovedFromHunter enemyId]
                , CheckWindow [leadId] [mkAfter $ Window.EnemyMovesTo lid MovedViaHunter enemyId]
                ]
              pure $ a & movedFromHunterKeywordL .~ True
            ls -> do
              push
                $ chooseOrRunOne
                  lead
                  [ targetLabel
                    l
                    [ EnemyMove enemyId l
                    , CheckWindow [leadId] [mkAfter $ Window.MovedFromHunter enemyId]
                    , CheckWindow [leadId] [mkAfter $ Window.EnemyMovesTo l MovedViaHunter enemyId]
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
            select $ NearestLocationToLocation loc (locationMatcherModifier lMatcher)

          (leadId, lead) <- getLeadInvestigatorPlayer
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
                  CheckWindow [leadId] [mkAfter $ Window.EnemyMovesTo lid MovedViaOther enemyId]
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
                      CheckWindow [leadId] [mkAfter $ Window.EnemyMovesTo l MovedViaOther enemyId]
                    ]
                  | l <- ls
                  ]
          pure a
    EnemiesAttack | not enemyExhausted -> do
      mods <- getModifiers (EnemyTarget enemyId)
      unless (CannotAttack `elem` mods) do
        iids <- select $ enemyAttacks
        for_ iids \iid ->
          push
            $ EnemyWillAttack
            $ (enemyAttack enemyId a iid)
              { attackDamageStrategy = enemyDamageStrategy
              , attackExhaustsEnemy = True
              }
      pure a
    AttackEnemy iid eid source mTarget skillType | eid == enemyId -> do
      enemyFight' <- fieldJust EnemyFight eid
      push
        $ fight iid source (maybe (toTarget eid) (ProxyTarget (toTarget eid)) mTarget) skillType enemyFight'
      pure a
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isActionTarget a target -> do
      whenWindow <- checkWindows [mkWhen (Window.SuccessfulAttackEnemy iid enemyId n)]
      afterSuccessfulWindow <- checkWindows [mkAfter (Window.SuccessfulAttackEnemy iid enemyId n)]
      afterWindow <- checkWindows [mkAfter (Window.EnemyAttacked iid source enemyId)]
      pushAll
        [ whenWindow
        , Successful (Action.Fight, toProxyTarget target) iid source (toActionTarget target) n
        , afterSuccessfulWindow
        , afterWindow
        ]

      pure a
    Successful (Action.Fight, _) iid source target _ | isTarget a target -> do
      push $ InvestigatorDamageEnemy iid enemyId source
      pure a
    FailedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isTarget a target -> do
      pushAll
        [ FailedAttackEnemy iid enemyId
        , CheckWindow [iid] [mkAfter $ Window.FailAttackEnemy iid enemyId n]
        , CheckWindow [iid] [mkAfter $ Window.EnemyAttacked iid source enemyId]
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
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyEvaded iid eid'
          pure a
        _ -> do
          mods <- getModifiers (InvestigatorTarget iid)
          lid <- fieldJust EnemyLocation eid
          let
            updatePlacement =
              if DoNotDisengageEvaded `elem` mods
                then id
                else placementL .~ AtLocation lid
            updateExhausted =
              if DoNotExhaustEvaded `elem` mods
                then id
                else exhaustedL .~ True
          pure $ a & updatePlacement & updateExhausted
    Exhaust (isTarget a -> True) -> do
      afterWindow <- checkWindows [mkAfter $ Window.Exhausts (toTarget a)]
      push afterWindow
      case enemyPlacement of
        AsSwarm eid' _ -> do
          others <- select $ SwarmOf eid' <> not_ (EnemyWithId $ toId a) <> ReadyEnemy
          pushAll [Exhaust (toTarget other) | other <- others]
        _ -> do
          others <- select $ SwarmOf (toId a) <> ReadyEnemy
          pushAll [Exhaust (toTarget other) | other <- others]
      pure $ a & exhaustedL .~ True
    TryEvadeEnemy iid eid source mTarget skillType | eid == enemyId -> do
      mEnemyEvade' <- field EnemyEvade eid
      case mEnemyEvade' of
        Just n -> push $ evade iid source (maybe (toTarget eid) (ProxyTarget (toTarget eid)) mTarget) skillType n
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
    Successful (Action.Evade, _) iid _ target _ | isTarget a target -> do
      push $ EnemyEvaded iid enemyId
      pure a
    FailedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isTarget a target -> do
      whenWindow <- checkWindows [mkWhen $ Window.FailEvadeEnemy iid enemyId n]
      afterWindow <- checkWindows [mkAfter $ Window.FailEvadeEnemy iid enemyId n]
      pushAll
        [ whenWindow
        , Failed (Action.Evade, toProxyTarget target) iid source (toActionTarget target) n
        , afterWindow
        ]
      pure a
    Failed (Action.Evade, _) iid _ target _ | isTarget a target -> do
      keywords <- getModifiedKeywords a
      pushAll
        [ EnemyAttack $ viaAlert $ (enemyAttack enemyId a iid) {attackDamageStrategy = enemyDamageStrategy}
        | Keyword.Alert `elem` keywords
        ]
      pure a
    InitiateEnemyAttack details | attackEnemy details == enemyId -> do
      push $ EnemyAttack details
      pure a
    EnemyAttack details | attackEnemy details == enemyId -> do
      whenAttacksWindow <- checkWindows [mkWhen $ Window.EnemyAttacks details]
      afterAttacksEventIfCancelledWindow <-
        checkWindows [mkAfter $ Window.EnemyAttacksEvenIfCancelled details]
      whenWouldAttackWindow <- checkWindows [mkWhen $ Window.EnemyWouldAttack details]
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
          canceled <- elem enemyId <$> select n
          pure $ if canceled then c : cards else cards
        applyModifiers m _ = pure m

      cardsThatCanceled <- foldM applyModifiers [] modifiers

      ignoreWindows <- for cardsThatCanceled \card ->
        checkWindows [mkAfter $ Window.CancelledOrIgnoredCardOrGameEffect $ CardSource card]

      let
        allowAttack =
          and
            [ null cardsThatCanceled
            , EffectsCannotBeCanceled `notElem` sourceModifiers && attackCanBeCanceled details
            ]

      healthDamage <- field EnemyHealthDamage (toId a)
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
            <> [Exhaust (toTarget a) | allowAttack, attackExhaustsEnemy details]
            <> ignoreWindows
            <> [After (EnemyAttack details)]
        _ -> error "Unhandled"
      pure a
    After (EnemyAttack details) | attackEnemy details == toId a -> do
      afterAttacksWindow <- checkWindows [mkAfter $ Window.EnemyAttacks details]
      push afterAttacksWindow
      pure a
    HealDamage (EnemyTarget eid) source n | eid == enemyId -> do
      afterWindow <- checkWindows [mkAfter $ Window.Healed DamageType (toTarget a) source n]
      push afterWindow
      pure $ a & tokensL %~ subtractTokens Token.Damage n
    HealAllDamage (EnemyTarget eid) source | eid == enemyId -> do
      afterWindow <-
        checkWindows [mkAfter $ Window.Healed DamageType (toTarget a) source (enemyDamage a)]
      push afterWindow
      pure $ a & tokensL %~ removeAllTokens Token.Damage
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
      let
        direct = damageAssignmentDirect damageAssignment
        source = damageAssignmentSource damageAssignment
        amount = damageAssignmentAmount damageAssignment
      canDamage <- sourceCanDamageEnemy eid source
      if canDamage
        then do
          amount' <- getModifiedDamageAmount a direct amount
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
            modifiedHealth <- fieldJust EnemyHealth (toId a)
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
            modifiedHealth <- fieldJust EnemyHealth (toId a)
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
          pure $ a & tokensL %~ addTokens Token.Damage amount' & assignedDamageL .~ mempty
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
      vengeance <- getVengeancePoints eid
      let
        victoryMsgs = [DefeatedAddToVictory $ toTarget a | isJust (victory <|> vengeance)]
        defeatMsgs =
          if isJust (victory <|> vengeance)
            then resolve $ RemoveEnemy eid
            else [Discard miid GameSource $ toTarget a]

      withQueue_ $ mapMaybe (filterOutEnemyMessages eid)
      discardWindow <- windows [Window.EntityDiscarded source (toTarget a)]

      pushAll
        $ [whenMsg, When msg, After msg]
        <> victoryMsgs
        <> [afterMsg]
        <> discardWindow
        <> defeatMsgs
      pure $ a & keysL .~ mempty
    After (EnemyDefeated eid _ _ _) | eid == toId a -> do
      pure $ a & defeatedL .~ True
    Discard miid source target | a `isTarget` target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      windows'' <- windows [Window.EntityDiscarded source (toTarget a)]
      let
        card = case enemyPlacement of
          AsSwarm _ c -> c
          _ -> toCard a
      pushAll
        $ windows'
        <> windows''
        <> [ RemovedFromPlay $ toSource a
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
      windowMsg <- checkWindows $ ($ Window.LeavePlay (toTarget a)) <$> [mkWhen, mkAfter]
      pushAll
        $ windowMsg
        : map (toDiscard GameSource) enemyAssets
          <> [UnsealChaosToken token | token <- enemySealedChaosTokens]
      pure a
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EnemyEngageInvestigator eid' iid
        _ -> do
          lid <- getJustLocation iid
          enemyLocation <- field EnemyLocation eid
          when (Just lid /= enemyLocation) $ push $ EnemyEntered eid lid
          massive <- eid <=~> MassiveEnemy
          pushWhen (not massive) $ PlaceEnemy eid (InThreatArea iid)
      pure a
    EngageEnemy iid eid mTarget False | eid == enemyId -> do
      case enemyPlacement of
        AsSwarm eid' _ -> do
          push $ EngageEnemy iid eid' mTarget False
        _ -> do
          massive <- eid <=~> MassiveEnemy
          pushWhen (not massive) $ PlaceEnemy eid (InThreatArea iid)
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
    CheckAttackOfOpportunity iid isFast | not isFast && not enemyExhausted -> do
      willAttack <- elem iid <$> select (investigatorEngagedWith enemyId)
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
      mods <- getModifiers enemyId
      let
        getModifiedSpawnAt [] = enemySpawnAt
        getModifiedSpawnAt (ForceSpawnLocation m : _) = Just $ SpawnAt m
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
                      spawnAt
                        enemyId
                        (Just iid)
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
          pushAll $ windows' : resolve (EnemySpawn miid lid eid)
        xs -> spawnAtOneOf miid eid xs
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
      cannotPlaceDoom <- hasModifier a CannotPlaceDoomOnThis
      if cannotPlaceDoom
        then pure a
        else do
          pushAllM $ windows [Window.PlacedDoom source (toTarget a) amount]
          pure $ a & tokensL %~ addTokens Doom amount
    PlaceTokens source target token n | isTarget a target -> do
      case token of
        Clue -> pushAllM $ windows [Window.PlacedClues source (toTarget a) n]
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
      checkEntersThreatArea a placement
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
      pushAll $ map (`checkDefeated` a) (keys enemyAssignedDamage)
      pure a
    Msg.Damage (isTarget a -> True) _ _ -> do
      error $ "Use EnemyDamage instead"
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
    PlaceUnderneath _ cards -> do
      when (toCard a `elem` cards) $ push $ RemoveEnemy (toId a)
      pure a
    DoBatch _ msg' -> do
      -- generic DoBatch handler
      runMessage (Do msg') a
    _ -> pure a
