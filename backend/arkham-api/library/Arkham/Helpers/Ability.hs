{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Helpers.Ability where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Asset.Types (Field (..))
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Customization
import Arkham.Game.Settings
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Helpers.Cost (getCanAffordCost)
import {-# SOURCE #-} Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Modifiers (getModifiers, withoutModifier)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Helpers.Window (getThatEnemy, windowMatches)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

getAbility :: HasGame m => AbilityRef -> m (Maybe Ability)
getAbility ref = selectOne (Matcher.AbilityIs ref.source ref.index)

getCanPerformAbility
  :: (HasCallStack, HasGame m) => InvestigatorId -> [Window] -> Ability -> m Bool
getCanPerformAbility !iid !ws !ability = do
  -- can perform an ability means you can afford it
  -- it is in the right window
  -- passes restrictions

  abilityModifiers <- getModifiers (AbilityTarget iid ability.ref)

  let
    mThatEnemy = getThatEnemy ws
    fixEnemy = maybe id Matcher.replaceThatEnemy mThatEnemy
    actions = abilityActions ability
    additionalCosts =
      abilityAdditionalCosts ability <> flip mapMaybe abilityModifiers \case
        AdditionalCost x -> Just x
        _ -> Nothing
    cost = (`applyCostModifiers` abilityModifiers) $ fixEnemy $ abilityCost ability
    criteria = foldr setCriteria (abilityCriteria ability) abilityModifiers
    setCriteria :: ModifierType -> Criterion -> Criterion
    setCriteria = \case
      SetAbilityCriteria (CriteriaOverride c) -> const c
      _ -> id
    abWindow = case ability.source.location of
      Nothing -> abilityWindow ability
      Just lid -> Matcher.replaceThisLocation lid $ abilityWindow ability

  let
    debug :: Show a => a -> a
    debug = if ability.cardCode == "04073" && iid == "04002" then traceShowId else id

  debug ability `seq` pure ()

  debug <$> andM
    [ getCanAffordCost iid ability.source actions ws (debug $ mconcat $ cost : additionalCosts)
    , meetsActionRestrictions iid ws ability
    , anyM (\window -> windowMatches iid (toSource ability) window abWindow) ws
    , withActiveInvestigator iid (passesCriteria iid Nothing ability.source ability.requestor ws criteria)
    , not <$> preventedByInvestigatorModifiers iid ability
    ]

preventedByInvestigatorModifiers
  :: HasGame m => InvestigatorId -> Ability -> m Bool
preventedByInvestigatorModifiers iid ability = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  isForced <- isForcedAbility iid ability
  if isForced then pure False else anyM prevents modifiers
 where
  prevents = \case
    CannotTakeAction x -> preventsAbility x
    MustTakeAction x -> not <$> preventsAbility x -- reads a little weird but we want only thing things x would prevent with cannot take action
    _ -> pure False
  preventsAbility = \case
    IsAnyAction -> pure True
    FirstOneOfPerformed as ->
      if any (`elem` as) (abilityActions ability)
        then fieldP InvestigatorActionsTaken (\taken -> all (\a -> all (notElem a) taken) as) iid
        else pure False
    IsAction Action.Activate -> pure $ abilityIsActivate ability
    IsAction a -> pure $ a `elem` abilityActions ability
    AnyActionTarget as -> anyM preventsAbility as
    EnemyAction a matcher -> case abilitySource ability of
      EnemySource eid ->
        if a `elem` abilityActions ability then eid <=~> matcher else pure False
      _ -> pure False

meetsActionRestrictions
  :: HasGame m => InvestigatorId -> [Window] -> Ability -> m Bool
meetsActionRestrictions iid _ ab@Ability {..} = go abilityType
 where
  go = \case
    Haunted -> pure False
    Cosmos -> pure False
    Objective aType -> go aType
    DelayedAbility aType -> go aType
    ForcedWhen _ aType -> go aType
    ActionAbilityWithSkill actions _ cost -> go $ ActionAbility actions cost
    ActionAbility [] _ -> pure True
    ActionAbility actions _ -> anyM (canDoAction iid ab) actions
    FastAbility' _ [] -> pure True
    FastAbility' _ actions -> anyM (canDoAction iid ab) actions
    CustomizationReaction {} -> pure True
    ConstantReaction {} -> pure True
    ReactionAbility _ _ -> pure True
    ForcedAbility _ -> pure True
    SilentForcedAbility _ -> pure True
    ForcedAbilityWithCost _ _ -> pure True
    AbilityEffect {} -> pure True
    ServitorAbility _ -> pure True
    ConstantAbility -> pure False

canDoAction :: (HasCallStack, HasGame m) => InvestigatorId -> Ability -> Action -> m Bool
canDoAction iid ab@Ability {abilitySource, abilityIndex} = \case
  Action.Fight -> case abilitySource of
    LocationSource _lid -> pure True
    EnemySource eid -> do
      modifiers' <- getModifiers iid
      let valid = maybe True (== eid) $ listToMaybe [x | MustFight x <- modifiers']
      if not valid
        then pure False
        else do
          mods <- getModifiers eid
          let restrictions = concat [rs | CanOnlyBeAttackedByAbilityOn rs <- mods]
          if null restrictions
            then pure True
            else case ab.requestor.asset of
              Just aid -> do
                cardCode <- field AssetCardCode aid
                pure $ cardCode `elem` restrictions
              _ -> pure False
    _ -> do
      modifiers <- getModifiers $ AbilityTarget iid ab.ref
      let
        isOverride = \case
          EnemyFightActionCriteria override -> Just override
          CanModify (EnemyFightActionCriteria override) -> Just override
          _ -> Nothing
        overrides = mapMaybe isOverride modifiers
      enemies <- selectAny $ case nonEmpty overrides of
        Nothing -> Matcher.CanFightEnemy $ AbilitySource abilitySource abilityIndex
        Just os -> Matcher.CanFightEnemyWithOverride $ combineOverrides os
      canMoveToConnected <- case ab.source.asset of
        Just aid -> aid <=~> Matcher.AssetWithCustomization InscriptionOfTheHunt
        _ -> pure False
      locations <-
        selectAny
          $ Matcher.LocationWithModifier CanBeAttackedAsIfEnemy
          <> if canMoveToConnected
            then Matcher.orConnected (Matcher.locationWithInvestigator iid)
            else Matcher.locationWithInvestigator iid
      pure $ enemies || locations
  Action.Evade -> case abilitySource of
    EnemySource _ -> pure True
    _ -> do
      modifiers <- getModifiers (AbilityTarget iid ab.ref)
      let
        isOverride = \case
          EnemyEvadeActionCriteria override -> Just override
          CanModify (EnemyEvadeActionCriteria override) -> Just override
          _ -> Nothing
        overrides = mapMaybe isOverride modifiers
      selectAny $ case nonEmpty overrides of
        Nothing -> Matcher.CanEvadeEnemy $ AbilitySource abilitySource abilityIndex
        Just os -> Matcher.CanEvadeEnemyWithOverride $ combineOverrides os
  Action.Engage -> case abilitySource of
    EnemySource _ -> pure True
    _ -> do
      modifiers <- getModifiers (AbilityTarget iid ab.ref)
      let
        isOverride = \case
          EnemyEngageActionCriteria override -> Just override
          CanModify (EnemyEngageActionCriteria override) -> Just override
          _ -> Nothing
        overrides = mapMaybe isOverride modifiers
      case overrides of
        [] -> notNull <$> select (Matcher.CanEngageEnemy $ AbilitySource abilitySource abilityIndex)
        [o] -> notNull <$> select (Matcher.CanEngageEnemyWithOverride o)
        _ -> error "multiple overrides found"
  Action.Parley -> case abilitySource of
    EnemySource eid -> eid <=~> Matcher.canParleyEnemy iid
    AssetSource _ -> pure True
    ActSource _ -> pure True
    IndexedSource _ (AssetSource _) -> pure True
    IndexedSource _ (LocationSource _) -> pure True
    ProxySource (AssetSource _) _ -> pure True
    ProxySource (LocationSource _) _ -> pure True
    LocationSource _ -> pure True
    _ -> selectAny (Matcher.canParleyEnemy iid)
  Action.Investigate -> case abilitySource of
    LocationSource lid -> withoutModifier iid (CannotInvestigateLocation lid)
    _ -> notNull <$> select Matcher.InvestigatableLocation
  -- The below actions may not be handled correctly yet
  Action.Activate -> pure True
  Action.Draw -> pure True
  Action.Move -> pure True
  Action.Play -> pure True
  Action.Resign -> pure True
  Action.Resource -> pure True
  Action.Explore ->
    andM
      [ iid <=~> Matcher.InvestigatorWithoutModifier CannotExplore
      , notNull <$> getScenarioDeck ExplorationDeck
      ]
  Action.Circle -> pure True

getCanAffordAbility
  :: (HasCallStack, HasGame m) => InvestigatorId -> Ability -> [Window] -> m Bool
getCanAffordAbility iid ability ws = do
  andM
    [ getCanAffordUse iid ability ws
    , getCanAffordAbilityCost iid ability ws
    ]

getCanAffordAbilityCost :: HasGame m => InvestigatorId -> Ability -> [Window] -> m Bool
getCanAffordAbilityCost iid a@Ability {..} ws = do
  modifiers <- getModifiers (AbilityTarget iid a.ref)
  doDelayAdditionalCosts <- case abilityDelayAdditionalCosts of
    Nothing -> pure False
    Just delay -> case delay of
      DelayAdditionalCosts -> pure True
      DelayAdditionalCostsWhen c -> passesCriteria iid Nothing abilitySource abilitySource [] c
  investigateCosts <-
    if #investigate `elem` abilityActions a
      then do
        case abilityMetadata of
          Just (InvestigateTargets matcher) -> do
            ls <- select (matcher <> Matcher.InvestigatableLocation)
            costs <- for ls $ \lid -> do
              mods <- getModifiers lid
              pure $ fold [m | not doDelayAdditionalCosts, AdditionalCostToInvestigate m <- mods]
            pure [OrCost costs | Free `notElem` costs]
          _ -> do
            field InvestigatorLocation iid >>= \case
              Just lid -> do
                mods <- getModifiers lid
                pure [m | not doDelayAdditionalCosts, AdditionalCostToInvestigate m <- mods]
              _ -> pure []
      else pure []
  enterCosts <-
    if #move `elem` abilityActions a
      then case a.source.location of
        Just lid -> do
          mods <- getModifiers lid
          imods <- getModifiers iid
          pcosts <- filterM ((lid <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
          pure
            $ map snd pcosts
            <> [m | not doDelayAdditionalCosts, AdditionalCostToEnter m <- mods]
        _ -> pure []
      else pure []
  resignCosts <-
    if #resign `elem` abilityActions a
      then do
        field InvestigatorLocation iid >>= \case
          Nothing -> pure []
          Just lid -> do
            mods <- getModifiers lid
            pure [m | AdditionalCostToResign m <- mods]
      else pure []
  let
    mThatEnemy = getThatEnemy ws
    fixEnemy = maybe id Matcher.replaceThatEnemy mThatEnemy
    costF =
      case find isSetCost modifiers of
        Just (SetAbilityCost c) -> fixEnemy . fold . (: investigateCosts <> resignCosts <> enterCosts) . const c
        _ -> fixEnemy . fold . (: investigateCosts <> resignCosts <> enterCosts)
    isSetCost = \case
      SetAbilityCost _ -> True
      _ -> False
  go (costF . (`applyCostModifiers` modifiers)) abilityType
 where
  go f = \case
    ServitorAbility _ -> pure True
    Haunted -> pure True
    Cosmos -> pure True
    ActionAbility actions cost ->
      getCanAffordCost iid (toSource a) actions ws (f cost)
    ActionAbilityWithSkill actions _ cost ->
      getCanAffordCost iid (toSource a) actions ws (f cost)
    ReactionAbility _ cost -> getCanAffordCost iid (toSource a) [] ws (f cost)
    CustomizationReaction _ _ cost -> getCanAffordCost iid (toSource a) [] ws (f cost)
    ConstantReaction _ _ cost -> getCanAffordCost iid (toSource a) [] ws (f cost)
    FastAbility' cost actions -> getCanAffordCost iid (toSource a) actions ws (f cost)
    ForcedAbilityWithCost _ cost ->
      getCanAffordCost iid (toSource a) [] ws (f cost)
    ForcedAbility _ -> pure True
    SilentForcedAbility _ -> pure True
    AbilityEffect actions cost ->
      getCanAffordCost iid (toSource a) actions ws (f cost)
    Objective {} -> pure True
    DelayedAbility inner -> go f inner
    ForcedWhen _ aType -> go f aType
    ConstantAbility -> pure False -- Never affordable, we should ignore

filterDepthSpecificAbilities :: HasGame m => [UsedAbility] -> m [UsedAbility]
filterDepthSpecificAbilities usedAbilities = do
  settings <- getSettings
  if settingsAbilitiesCannotReactToThemselves settings
    then pure usedAbilities
    else do
      depth <- getWindowDepth
      pure $ filter (valid depth) usedAbilities
 where
  valid depth ability =
    abilityLimitType (abilityLimit $ usedAbility ability)
      /= Just PerWindow
      || depth
      <= usedDepth ability

getAbilityLimit :: HasGame m => InvestigatorId -> Ability -> m AbilityLimit
getAbilityLimit iid ability = do
  ignoreLimit <- (IgnoreLimit `elem`) <$> getModifiers (AbilityTarget iid ability.ref)
  pure $ if ignoreLimit then PlayerLimit PerWindow 1 else abilityLimit ability

-- TODO: The limits that are correct are the one that check usedTimes Group
-- limits for instance won't work if we have a group limit higher than one, for
-- that we need to sum uses across all investigators. So we should fix this
-- soon.
getCanAffordUse :: (HasCallStack, HasGame m) => InvestigatorId -> Ability -> [Window] -> m Bool
getCanAffordUse = getCanAffordUseWith id CanIgnoreAbilityLimit

-- Use `f` to modify use count, used for `getWindowSkippable` to exclude the current call
-- EMAIL: Cards can't react to themselves, i.e. Grotesque Statue (4)
getCanAffordUseWith
  :: (HasCallStack, HasGame m)
  => ([UsedAbility] -> [UsedAbility])
  -> CanIgnoreAbilityLimit
  -> InvestigatorId
  -> Ability
  -> [Window]
  -> m Bool
getCanAffordUseWith f canIgnoreAbilityLimit iid ability ws = do
  usedAbilities <- fmap f . filterDepthSpecificAbilities =<< field InvestigatorUsedAbilities iid
  limit <- getAbilityLimit iid ability
  ignoreLimit <-
    or
      . sequence [(IgnoreLimit `elem`), (CanIgnoreLimit `elem`)]
      <$> getModifiers (AbilityTarget iid ability.ref)
  if ignoreLimit && canIgnoreAbilityLimit == CanIgnoreAbilityLimit
    then do
      case abilityType ability of
        ReactionAbility {} ->
          -- even if we are ignoring the limit we want to make sure that we've only used it once during this specific window
          pure $ notElem ability (map usedAbility $ filter usedThisWindow usedAbilities)
        _ -> pure True
    else case limit of
      NoLimit -> do
        let
          go = \case
            ReactionAbility _ _ ->
              pure $ notElem ability (map usedAbility usedAbilities)
            CustomizationReaction {} ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ConstantReaction {} ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ForcedWhen _ aType -> go aType
            ForcedAbility _ -> pure $ notElem ability (map usedAbility usedAbilities)
            SilentForcedAbility _ ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ForcedAbilityWithCost _ _ ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ActionAbility _ _ -> pure True
            ActionAbilityWithSkill {} -> pure True
            FastAbility' {} -> pure True
            AbilityEffect {} -> pure True
            Objective {} -> pure True
            DelayedAbility inner -> go inner
            Haunted -> pure True
            Cosmos -> pure True
            ServitorAbility _ -> pure True -- should be disabled by the servitor
            ConstantAbility -> pure False
        go (abilityType ability)
      PlayerLimit (PerSearch trait) n -> do
        let traitMatchingUsedAbilities = filter (elem trait . usedAbilityTraits) usedAbilities
        let usedCount = sum $ map usedTimes traitMatchingUsedAbilities
        pure $ usedCount < n
      PlayerLimit PerTestOrAbility n ->
        pure
          . (< n)
          . maybe 0 usedTimes
          $ find
            ((== ability) . usedAbility)
            usedAbilities
      PlayerLimit PerRound n -> do
        pure
          $ maybe True (and . sequence [not . usedThisWindow, (< n) . usedTimes])
          $ find ((== ability) . usedAbility) usedAbilities
      PlayerLimit _ n -> do
        pure
          $ maybe True (and . sequence [not . usedThisWindow, (< n) . usedTimes])
          $ find ((== ability) . usedAbility) usedAbilities
      MaxPer cardDef _ n -> do
        let
          abilityCardDef = \case
            MaxPer cDef _ _ -> Just cDef
            _ -> Nothing

        usedAbilities' <-
          filterDepthSpecificAbilities
            =<< concatMapM (field InvestigatorUsedAbilities)
            =<< allInvestigators

        let wasUsedThisWindow = maybe False usedThisWindow $ find ((== ability) . usedAbility) usedAbilities'

        pure
          . (&& not wasUsedThisWindow)
          . (< n)
          . getSum
          . foldMap (Sum . usedTimes)
          $ filter
            ((Just cardDef ==) . abilityCardDef . abilityLimit . usedAbility)
            usedAbilities'
      PerInvestigatorLimit _ n -> do
        -- This is difficult and based on the window, so we need to match out the
        -- relevant investigator ids from the window. If this becomes more prevalent
        -- we may want a method from `Window -> Maybe InvestigatorId`
        anyM
          ( \window ->
              case windowType window of
                Window.CommittedCard iid' _ -> do
                  let
                    matchingPerInvestigatorCount =
                      flip count usedAbilities $ \usedAbility' ->
                        flip any (usedAbilityWindows usedAbility') $ \case
                          (windowType -> Window.CommittedCard iid'' _) ->
                            usedAbility usedAbility' == ability && iid' == iid''
                          _ -> False
                  pure $ matchingPerInvestigatorCount < n
                _ -> pure False
          )
          ws
      GroupLimit _ n -> do
        usedAbilities' <-
          fmap (map usedAbility)
            . filterDepthSpecificAbilities
            =<< concatMapM (field InvestigatorUsedAbilities)
            =<< allInvestigators
        let total = count (== ability) usedAbilities'
        pure $ total < n

isForcedAbility :: HasGame m => InvestigatorId -> Ability -> m Bool
isForcedAbility iid Ability {abilitySource, abilityType} = isForcedAbilityType iid abilitySource abilityType

isForcedAbilityType :: HasGame m => InvestigatorId -> Source -> AbilityType -> m Bool
isForcedAbilityType iid source = \case
  SilentForcedAbility {} -> pure True
  ForcedAbility {} -> pure True
  ForcedAbilityWithCost {} -> pure True
  Objective aType -> isForcedAbilityType iid source aType
  DelayedAbility aType -> isForcedAbilityType iid source aType
  FastAbility' {} -> pure False
  ReactionAbility {} -> pure False
  CustomizationReaction {} -> pure True -- TODO: Keep an eye on this
  ConstantReaction {} -> pure False
  ActionAbility {} -> pure False
  ActionAbilityWithSkill {} -> pure False
  AbilityEffect {} -> pure False
  ServitorAbility {} -> pure False
  Haunted {} -> pure True -- Maybe? we wanted this to basically never be valid but still take forced precedence
  Cosmos {} -> pure True -- Maybe? we wanted this to basically never be valid but still take forced precedence
  ForcedWhen c _ -> passesCriteria iid Nothing source source [] c
  ConstantAbility {} -> pure False
