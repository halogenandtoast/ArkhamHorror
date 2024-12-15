module Arkham.Game.Helpers (module Arkham.Game.Helpers, module X) where

import Arkham.Prelude

import Arkham.Helpers.Ability as X
import Arkham.Helpers.Cost as X
import Arkham.Helpers.Doom as X
import Arkham.Helpers.EncounterSet as X
import Arkham.Helpers.GameValue as X
import Arkham.Helpers.Log as X
import Arkham.Helpers.Matchers as X
import Arkham.Helpers.Modifiers as X
import Arkham.Helpers.Query as X
import Arkham.Helpers.Ref as X
import Arkham.Helpers.Scenario as X
import Arkham.Helpers.Slot as X
import Arkham.Helpers.Source as X
import Arkham.Helpers.Target as X
import Arkham.Helpers.Window as X
import Arkham.Helpers.Xp as X

import Arkham.Ability
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Action.Additional (AdditionalActionType (BobJenkinsAction), additionalActionType)
import {-# SOURCE #-} Arkham.Asset (createAsset)
import Arkham.Asset.Types (Asset, AssetAttrs (assetCardCode), Field (..))
import Arkham.Attack
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes hiding (isMatch)
import Arkham.Classes.HasGame
import Arkham.Criteria qualified as Criteria
import Arkham.Customization
import Arkham.DamageEffect
import Arkham.Deck hiding (InvestigatorDeck, InvestigatorDiscard)
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Effect.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Event, Field (..))
import {-# SOURCE #-} Arkham.Game
import Arkham.Game.Settings
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Calculation
import Arkham.Helpers.Card
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator (
  baseSkillValueFor,
  getActionCost,
  getCanAfford,
 )
import Arkham.Helpers.Message hiding (InvestigatorDamage, PaidCost)
import Arkham.Helpers.SkillTest (getIsPerilous, getSkillTestDifficulty, isParley)
import Arkham.Helpers.Tarot
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Types (Field (..), Investigator, InvestigatorAttrs (..))
import Arkham.Key
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types hiding (location)
import Arkham.Matcher qualified as Matcher
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Types (Field (..), ScenarioDeckKey (ExplorationDeck))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos qualified as Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Skill.Types (Field (..))
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import Arkham.Story.Types (Field (..))
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait (toTraits)
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), defaultWindows, mkWhen)
import Arkham.Window qualified as Window
import Control.Lens (over)
import Control.Monad.Reader (local)
import Data.Data.Lens (biplate)
import Data.List qualified as List
import Data.List.Extra (nubOrdOn)
import Data.Set qualified as Set

replaceThisCard :: Card -> Source -> Source
replaceThisCard c = \case
  ThisCard -> CardCostSource (toCardId c)
  s -> s

cancelChaosToken :: HasQueue Message m => ChaosToken -> m ()
cancelChaosToken token = withQueue_ $ \queue ->
  mapMaybe
    ( \case
        When (RevealChaosToken _ _ token') | token == token' -> Nothing
        RevealChaosToken _ _ token' | token == token' -> Nothing
        After (RevealChaosToken _ _ token') | token == token' -> Nothing
        CheckWindows ws -> case filter (not . isRevealChaosToken) ws of
          [] -> Nothing
          ws' -> Just $ CheckWindows ws'
        Do (CheckWindows ws) -> case filter (not . isRevealChaosToken) ws of
          [] -> Nothing
          ws' -> Just $ Do (CheckWindows ws')
        RequestedChaosTokens s miid ts -> Just $ RequestedChaosTokens s miid (filter (/= token) ts)
        msg -> Just msg
    )
    queue
 where
  isRevealChaosToken w = case windowType w of
    Window.RevealChaosToken _ token' -> token == token'
    _ -> False

getPlayableCards
  :: (HasCallStack, HasGame m) => InvestigatorAttrs -> CostStatus -> [Window] -> m [Card]
getPlayableCards a costStatus windows' = do
  asIfInHandCards <- getAsIfInHandCards (toId a)
  otherPlayersPlayableCards <- getOtherPlayersPlayableCards (toId a) costStatus windows'
  playableDiscards <- getPlayableDiscards a costStatus windows'
  hand <- field InvestigatorHand (toId a)
  playableHandCards <-
    filterM (getIsPlayable (toId a) (toSource a) costStatus windows') (hand <> asIfInHandCards)
  pure $ playableHandCards <> playableDiscards <> otherPlayersPlayableCards

getPlayableDiscards :: HasGame m => InvestigatorAttrs -> CostStatus -> [Window] -> m [Card]
getPlayableDiscards attrs@InvestigatorAttrs {..} costStatus windows' = do
  modifiers <- getModifiers (toTarget attrs)
  filterM
    (getIsPlayable (toId attrs) (toSource attrs) costStatus windows')
    (possibleCards modifiers)
 where
  possibleCards modifiers =
    map (PlayerCard . snd)
      $ filter
        (canPlayFromDiscard modifiers)
        (withIndex investigatorDiscard)
  canPlayFromDiscard modifiers (n, card) =
    cdPlayableFromDiscard (toCardDef card)
      || any (allowsPlayFromDiscard n card) modifiers
  allowsPlayFromDiscard 0 card (CanPlayTopmostOfDiscard (mcardType, traits)) =
    let cardMatcher = maybe Matcher.AnyCard Matcher.CardWithType mcardType <> foldMap Matcher.CardWithTrait traits
        allMatches = filter (`cardMatch` cardMatcher) investigatorDiscard
     in case allMatches of
          (topmost : _) -> topmost == card
          _ -> False
  allowsPlayFromDiscard 0 card (CanPlayFromDiscard (mcardType, traits)) =
    let cardMatcher = maybe Matcher.AnyCard Matcher.CardWithType mcardType <> foldMap Matcher.CardWithTrait traits
        allMatches = filter (`cardMatch` cardMatcher) investigatorDiscard
     in card `elem` allMatches
  allowsPlayFromDiscard _ _ _ = False

getAsIfInHandCards :: (HasCallStack, HasGame m) => InvestigatorId -> m [Card]
getAsIfInHandCards iid = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  let
    modifiersPermitPlayOfDiscard discard c =
      any (modifierPermitsPlayOfDiscard discard c) modifiers
    modifierPermitsPlayOfDiscard discard (c, _) = \case
      CanPlayTopmostOfDiscard (mType, traits) ->
        let cardMatcher = maybe Matcher.AnyCard Matcher.CardWithType mType <> foldMap Matcher.CardWithTrait traits
            allMatches = filter (`cardMatch` cardMatcher) discard
         in case allMatches of
              (topmost : _) -> topmost == c
              _ -> False
      _ -> False
    modifiersPermitPlayOfDeck c = any (modifierPermitsPlayOfDeck c) modifiers
    modifierPermitsPlayOfDeck (c, depth) = \case
      CanPlayTopOfDeck cardMatcher | depth == 0 -> cardMatch c cardMatcher
      _ -> False
    cardsAddedViaModifiers = flip mapMaybe modifiers $ \case
      AsIfInHand c -> Just c
      _ -> Nothing
  discard <- field InvestigatorDiscard iid
  deck <- fieldMap InvestigatorDeck unDeck iid
  pure
    $ map
      (PlayerCard . fst)
      (filter (modifiersPermitPlayOfDiscard discard) (zip discard [0 :: Int ..]))
    <> map
      (PlayerCard . fst)
      (filter modifiersPermitPlayOfDeck (zip deck [0 :: Int ..]))
    <> cardsAddedViaModifiers

getCanPerformAbility
  :: (HasCallStack, HasGame m) => InvestigatorId -> [Window] -> Ability -> m Bool
getCanPerformAbility !iid !ws !ability = do
  -- can perform an ability means you can afford it
  -- it is in the right window
  -- passes restrictions

  abilityModifiers <- getModifiers (AbilityTarget iid ability)
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

  andM
    [ getCanAffordCost iid (toSource ability) actions ws cost
    , meetsActionRestrictions iid ws ability
    , anyM (\window -> windowMatches iid (toSource ability) window (abilityWindow ability)) ws
    , withActiveInvestigator iid (passesCriteria iid Nothing ability.source ability.requestor ws criteria)
    , allM (getCanAffordCost iid (abilitySource ability) actions ws) additionalCosts
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

canDoAction :: (HasCallStack, HasGame m) => InvestigatorId -> Ability -> Action -> m Bool
canDoAction iid ab@Ability {abilitySource, abilityIndex} = \case
  Action.Fight -> case abilitySource of
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
      modifiers <- getModifiers (AbilityTarget iid ab)
      let
        isOverride = \case
          EnemyFightActionCriteria override -> Just override
          CanModify (EnemyFightActionCriteria override) -> Just override
          _ -> Nothing
        overrides = mapMaybe isOverride modifiers
      case overrides of
        [] -> notNull <$> select (Matcher.CanFightEnemy $ AbilitySource abilitySource abilityIndex)
        [o] -> notNull <$> select (Matcher.CanFightEnemyWithOverride o)
        _ -> error "multiple overrides found"
  Action.Evade -> case abilitySource of
    EnemySource _ -> pure True
    _ -> do
      modifiers <- getModifiers (AbilityTarget iid ab)
      let
        isOverride = \case
          EnemyEvadeActionCriteria override -> Just override
          CanModify (EnemyEvadeActionCriteria override) -> Just override
          _ -> Nothing
        overrides = mapMaybe isOverride modifiers
      case overrides of
        [] -> notNull <$> select (Matcher.CanEvadeEnemy $ AbilitySource abilitySource abilityIndex)
        [o] -> notNull <$> select (Matcher.CanEvadeEnemyWithOverride o)
        _ -> error "multiple overrides found"
  Action.Engage -> case abilitySource of
    EnemySource _ -> pure True
    _ -> do
      modifiers <- getModifiers (AbilityTarget iid ab)
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
  modifiers <- getModifiers (AbilityTarget iid a)
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
  ignoreLimit <-
    (IgnoreLimit `elem`)
      <$> getModifiers (AbilityTarget iid ability)
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
      <$> getModifiers (AbilityTarget iid ability)
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
      PlayerLimit _ n ->
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

getActions :: (HasGame m, HasCallStack) => InvestigatorId -> [Window] -> m [Ability]
getActions iid ws = getActionsWith iid ws id

getActionsWith
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> [Window]
  -> (Ability -> Ability)
  -> m [Ability]
getActionsWith iid ws f = do
  modifiersForFilter <- getModifiers iid
  let
    abilityFilters =
      mapMaybe
        ( \case
            CannotTriggerAbilityMatching m -> Just (Matcher.TriggeredAbility <> m)
            _ -> Nothing
        )
        modifiersForFilter

  unfilteredActions <- map f <$> getAllAbilities
  actions' <-
    if null abilityFilters
      then pure unfilteredActions
      else do
        ignored <- select (mconcat abilityFilters)
        pure $ filter (`notElem` ignored) unfilteredActions
  actionsWithSources <-
    concat <$> for actions' \action -> do
      case abilitySource action of
        ProxySource (AgendaMatcherSource m) base -> do
          sources <- selectMap AgendaSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (ActMatcherSource m) base -> do
          sources <- selectMap ActSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (AssetMatcherSource m) base -> do
          sources <- selectMap AssetSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (LocationMatcherSource m) base -> do
          sources <- selectMap LocationSource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        ProxySource (EnemyMatcherSource m) base -> do
          sources <- selectMap EnemySource m
          pure
            $ map
              (\source -> action {abilitySource = ProxySource source base})
              sources
        _ -> pure [action]

  actions'' <-
    catMaybes <$> for
      actionsWithSources
      \ability -> do
        modifiers' <- getModifiers (sourceToTarget $ abilitySource ability)
        investigatorModifiers <- getModifiers (InvestigatorTarget iid)
        let bountiesOnly = BountiesOnly `elem` investigatorModifiers
        cardClasses <- case abilitySource ability of
          AssetSource aid -> field AssetClasses aid
          _ -> pure $ singleton Neutral

        -- if enemy only bounty enemies
        sourceIsBounty <- case abilitySource ability of
          EnemySource eid -> eid <=~> Matcher.EnemyWithBounty
          _ -> pure True

        isForced <- isForcedAbility iid ability
        let
          -- Lola Hayes: Forced abilities will always trigger
          prevents (CanOnlyUseCardsInRole role) =
            null (setFromList [role, Neutral] `intersect` cardClasses)
              && not isForced
          prevents CannotTriggerFastAbilities = isFastAbility ability
          prevents _ = False
          -- Blank excludes any non-default abilities (index >= 100)
          -- TODO: this is a leaky abstraction, we may want to track this
          -- on the abilities themselves
          blankPrevents Blank = abilityIndex ability < 100
          blankPrevents BlankExceptForcedAbilities = not isForced
          blankPrevents _ = False
          -- If the window is fast we only permit fast abilities, but forced
          -- abilities need to be everpresent so we include them
          needsToBeFast =
            all
              ( \window ->
                  windowType window
                    == Window.FastPlayerWindow
                    && not
                      ( isFastAbility ability
                          || isForced
                          || isReactionAbility ability
                      )
              )
              ws

        pure
          $ if any prevents investigatorModifiers
            || any blankPrevents modifiers'
            || needsToBeFast
            || (bountiesOnly && not sourceIsBounty)
            then Nothing
            else Just $ applyAbilityModifiers ability modifiers'

  actions''' <-
    filterM
      ( \action -> do
          andM
            [ getCanPerformAbility iid ws action
            , getCanAffordAbility iid action ws
            ]
      )
      actions''
  forcedActions <- filterM (isForcedAbility iid) actions'''
  pure $ nub $ if null forcedActions then actions''' else forcedActions

hasFightActions
  :: (Sourceable source, HasGame m)
  => InvestigatorId
  -> source
  -> Matcher.WindowMatcher
  -> [Window]
  -> m Bool
hasFightActions iid requestor window windows' =
  anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1)
    . map (setRequestor requestor)
    =<< select (Matcher.BasicAbility <> Matcher.AbilityIsAction #fight <> Matcher.AbilityWindow window)

hasEvadeActions
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> [Window]
  -> m Bool
hasEvadeActions iid window windows' =
  anyM (\a -> getCanPerformAbility iid windows' $ decreaseAbilityActionCost a 1)
    =<< select
      (Matcher.AbilityIsAction Action.Evade <> Matcher.AbilityWindow window)

getIsPlayable
  :: (HasCallStack, HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable iid source costStatus windows' c = do
  availableResources <- getSpendableResources iid
  getIsPlayableWithResources iid source availableResources costStatus windows' c

withAlteredGame :: HasGame m => (Game -> Game) -> ReaderT Game m a -> m a
withAlteredGame f body = do
  game <- getGame
  runReaderT body (f game)

withDepthGuard :: HasGame m => Int -> a -> ReaderT Game m a -> m a
withDepthGuard maxDepth defaultValue body = do
  game <- getGame
  flip runReaderT game $ do
    depth <- getDepthLock
    if depth > maxDepth then pure defaultValue else local delve body

getIsPlayableWithResources
  :: forall m source
   . (HasCallStack, HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayableWithResources _ _ _ _ _ (VengeanceCard _) = pure False
getIsPlayableWithResources _ _ _ _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayableWithResources iid (toSource -> source) availableResources costStatus windows' c@(PlayerCard _) = do
  if c.kind `elem` [PlayerTreacheryType, PlayerEnemyType]
    then pure False
    else do
      ignoreContexts <- hasModifier iid IgnorePlayableModifierContexts
      contexts :: [(Matcher.CardMatcher, [ModifierType])] <-
        concat . mapMaybe (preview _PlayableModifierContexts) <$> getModifiers iid
      base <- go @m
      others <-
        traverse
          (\(matcher, ctx) -> (cardMatch c matcher &&) <$> withModifiers iid (toModifiers iid ctx) go)
          (if ignoreContexts then [] else contexts)
      pure $ or (base : others)
 where
  pcDef = toCardDef c
  prevents (CanOnlyUseCardsInRole role) =
    null $ intersect (cdClassSymbols pcDef) (setFromList [Neutral, role])
  prevents (CannotPlay matcher) = cardMatch c matcher
  prevents (CannotPutIntoPlay matcher) = cardMatch c matcher
  prevents _ = False
  go :: forall n. HasGame n => n Bool
  go = withDepthGuard 3 False $ do
    attrs <- getAttrs @Investigator iid
    isBobJenkins <- case source of
      AbilitySource (InvestigatorSource "08016") 1 -> do
        case toCardOwner c of
          Just owner ->
            andM
              [ pure $ c `cardMatch` card_ (#asset <> #item)
              , pure
                  $ BobJenkinsAction
                  `notElem` map additionalActionType (investigatorUsedAdditionalActions attrs)
              , owner <=~> Matcher.affectsOthers (Matcher.colocatedWith @InvestigatorId "08016")
              ]
          Nothing -> pure False
      _ -> pure False
    iids <- filter (/= iid) <$> getInvestigators
    iidsWithModifiers <- for iids $ \iid' -> (iid',) <$> getModifiers iid'
    canHelpPay <- flip filterM iidsWithModifiers \(iid', modifiers') -> do
      bobJenkinsPermitted <-
        if isBobJenkins
          then do
            case toCardOwner c of
              Just owner | owner == iid' -> pure True
              _ -> pure False
          else pure False
      modifierPermitted <- flip anyM modifiers' $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher
          | cardMatch c cMatcher && CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers' ->
              iid <=~> iMatcher
        _ -> pure False
      pure $ bobJenkinsPermitted || modifierPermitted

    modifiers <- getModifiers iid
    resourcesFromAssets <-
      sum <$> for ((iid, modifiers) : iidsWithModifiers) \(iid', modifiers') -> do
        sum <$> for modifiers' \case
          CanSpendUsesAsResourceOnCardFromInvestigator assetId uType iMatcher cMatcher | cardMatch c cMatcher -> do
            let canAffect = iid == iid' || CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers'
            canContribute <- (canAffect &&) <$> iid <=~> iMatcher
            if canContribute
              then fieldMap AssetUses (findWithDefault 0 uType) assetId
              else pure 0
          _ -> pure 0

    additionalResources <-
      (resourcesFromAssets +)
        . sum
        <$> traverse (field InvestigatorResources . fst) canHelpPay
    cardModifiers <- getModifiers c
    let title = nameTitle (cdName pcDef)
    passesUnique <- case (cdUnique pcDef, cdCardType pcDef) of
      (True, AssetType) ->
        not <$> case nameSubtitle (cdName pcDef) of
          Nothing -> selectAny (Matcher.AssetWithTitle title)
          Just subtitle -> selectAny (Matcher.AssetWithFullTitle title subtitle)
      _ -> pure True

    baseModifiedCardCost <- getModifiedCardCost iid c
    modifiedCardCost <- getPotentiallyModifiedCardCost iid c True baseModifiedCardCost
    modifiedCardCostWithChuckFergus <- getPotentiallyModifiedCardCost iid c False baseModifiedCardCost

    let
      alternateResourceCost = \case
        AlternateResourceCost cardMatcher cost | c `cardMatch` cardMatcher -> Just cost
        CanModify (AlternateResourceCost cardMatcher cost) | c `cardMatch` cardMatcher -> Just cost
        _ -> Nothing
      alternateResourceCosts = mapMaybe alternateResourceCost modifiers

    canAffordAlternateResourceCost <- case alternateResourceCosts of
      [] -> pure False
      _ -> anyM (getCanAffordCost iid source (cdActions $ toCardDef c) windows') alternateResourceCosts

    let
      auxiliaryCosts =
        case costStatus of
          AuxiliaryCost x _ -> [x]
          _ -> []

      auxiliaryResourceCosts = totalResourceCost (mconcat auxiliaryCosts)
      replaceThisCardSource :: Data a => a -> a
      replaceThisCardSource = over biplate (replaceThisCard c)
      canAffordCost' = modifiedCardCost + auxiliaryResourceCosts <= availableResources + additionalResources
      canAffordCost =
        if canAffordCost'
          then canAffordCost'
          else
            modifiedCardCostWithChuckFergus
              + auxiliaryResourceCosts
              <= (availableResources + additionalResources)
      needsChuckFergus = not canAffordCost' && canAffordCost
      handleCriteriaReplacement _ (CanPlayWithOverride (Criteria.CriteriaOverride cOverride)) = Just cOverride
      handleCriteriaReplacement m _ = m
      duringTurnWindow = mkWhen (Window.DuringTurn iid)
      notFastWindow = any (`elem` windows') [duringTurnWindow]
      canBecomeFast =
        CannotPlay Matcher.FastCard
          `notElem` modifiers
          && foldr applyModifier False (nub $ cardModifiers <> modifiers)
      canBecomeFastWindow =
        guard canBecomeFast
          $> fromMaybe
            (Matcher.DuringTurn Matcher.You)
            (listToMaybe [w | BecomesFast w <- cardModifiers <> modifiers])
      applyModifier (BecomesFast _) _ = True
      applyModifier (CanBecomeFast cardMatcher) _ = cardMatch c cardMatcher
      applyModifier (ChuckFergus2Modifier cardMatcher _) _ = not needsChuckFergus && cardMatch c cardMatcher
      applyModifier _ val = val
      source' = replaceThisCardSource source
    passesCriterias <-
      maybe
        (pure True)
        (passesCriteria iid (Just (c, costStatus)) source' (CardIdSource c.id) windows')
        (foldl' handleCriteriaReplacement (replaceThisCardSource $ cdCriteria pcDef) cardModifiers)

    inFastWindow <-
      maybe
        (pure False)
        (cardInFastWindows iid source c windows')
        (cdFastWindow pcDef <|> canBecomeFastWindow)

    ac <- getActionCost attrs (cdActions pcDef)

    let
      isDuringTurnWindow = \case
        (windowType -> Window.DuringTurn iid') -> iid == iid'
        _ -> False
      doAsIfTurn = any isDuringTurnWindow windows'

    canEvade <- withGrantedActions iid GameSource ac do
      if (#evade `elem` cdActions pcDef)
        then
          if inFastWindow || doAsIfTurn
            then
              asIfTurn iid
                $ hasEvadeActions iid (Matcher.DuringTurn Matcher.You) (defaultWindows iid <> windows')
            else hasEvadeActions iid (Matcher.DuringTurn Matcher.You) (defaultWindows iid <> windows')
        else pure False

    canFight <- withGrantedActions iid GameSource ac do
      if (#fight `elem` pcDef.actions)
        then
          if inFastWindow || doAsIfTurn
            then
              asIfTurn iid
                $ hasFightActions
                  iid
                  (CardIdSource c.id)
                  (Matcher.DuringTurn Matcher.You)
                  (defaultWindows iid <> windows')
            else
              hasFightActions
                iid
                (CardIdSource c.id)
                (Matcher.DuringTurn Matcher.You)
                (defaultWindows iid <> windows')
        else pure False

    canInvestigate <-
      ((#investigate `elem` pcDef.actions) &&)
        <$> (maybe (pure False) (<=~> Matcher.InvestigatableLocation) =<< field InvestigatorLocation iid)

    passesLimits' <- passesLimits iid c

    let
      isPlayAction = \case
        PaidCost -> False
        UnpaidCost NoAction -> False
        UnpaidCost NeedsAction -> True
        AuxiliaryCost _ inner -> isPlayAction inner

    additionalCosts <-
      flip mapMaybeM (modifiers <> cardModifiers) \case
        AdditionalCost n -> pure (Just n)
        AdditionalActionCostOf match n -> do
          performedActions <- field InvestigatorActionsPerformed iid
          takenActions <- field InvestigatorActionsTaken iid
          let cardActions = if isPlayAction costStatus then #play : c.actions else c.actions
          pure
            $ guard (any (Matcher.matchTarget takenActions performedActions match) cardActions)
            $> ActionCost n
        _ -> pure Nothing

    let
      sealingToCost = \case
        Keyword.Sealing matcher -> Just $ SealCost matcher
        Keyword.SealUpTo n matcher -> Just $ UpTo (Fixed n) (SealCost matcher)
        Keyword.SealOneOf (m1 :| rest) -> Just $ OrCost $ mapMaybe sealingToCost (m1 : rest)
        Keyword.SealUpToX _ -> Nothing
      sealedChaosTokenCost = flip mapMaybe (setToList $ cdKeywords pcDef) $ \case
        Keyword.Seal sealing -> if costStatus == PaidCost then Nothing else sealingToCost sealing
        _ -> Nothing

    investigateCosts <-
      if #investigate `elem` cdActions pcDef
        then
          field InvestigatorLocation iid >>= \case
            Nothing -> pure []
            Just lid -> do
              mods <- getModifiers lid
              pure [m | AdditionalCostToInvestigate m <- mods]
        else pure []

    resignCosts <-
      if #resign `elem` cdActions pcDef
        then do
          mLocation <- field InvestigatorLocation iid
          case mLocation of
            Nothing -> pure []
            Just lid -> do
              mods <- getModifiers lid
              pure [m | AdditionalCostToResign m <- mods]
        else pure []

    -- NOTE: This used to be
    -- PaidCost -> pure . max 0 . subtract 1
    -- But we removed it because it conflicted with additional action costs
    let
      goActionCost = \case
        PaidCost -> const 0 ac
        UnpaidCost NoAction -> max 0 $ subtract 1 ac
        UnpaidCost NeedsAction -> ac
        AuxiliaryCost _ inner -> goActionCost inner
      actionCost = goActionCost costStatus

    canAffordAdditionalCosts <-
      getCanAffordCost iid (CardIdSource c.id) c.actions windows'
        $ fold
        $ [ActionCost actionCost | actionCost > 0 && not inFastWindow]
        <> additionalCosts
        <> auxiliaryCosts
        <> investigateCosts
        <> resignCosts
        <> sealedChaosTokenCost
        <> [fromMaybe mempty (cdAdditionalCost pcDef) | costStatus /= PaidCost]

    passesSlots <-
      if null (cdSlots pcDef)
        then pure True
        else do
          possibleSlots <- getPotentialSlots c iid
          pure $ null $ cdSlots pcDef \\ possibleSlots

    let
      goNoAction = \case
        UnpaidCost NoAction -> True
        AuxiliaryCost _ inner -> goNoAction inner
        _ -> False
      noAction = if isNothing (cdFastWindow pcDef) then goNoAction costStatus else False

    pure
      $ (cdCardType pcDef /= SkillType)
      && ((costStatus == PaidCost) || (canAffordCost || canAffordAlternateResourceCost))
      && (none prevents modifiers)
      && ((isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow || isBobJenkins || noAction)
      && ( (#evade `notElem` pcDef.actions)
            || canEvade
            || (cdOverrideActionPlayableIfCriteriaMet pcDef && #evade `elem` cdActions pcDef)
         )
      && ( (#fight `notElem` pcDef.actions)
            || canFight
            || (cdOverrideActionPlayableIfCriteriaMet pcDef && #fight `elem` cdActions pcDef)
         )
      && ((#investigate `notElem` cdActions pcDef) || canInvestigate)
      && passesCriterias
      && passesLimits'
      && passesUnique
      && passesSlots
      && canAffordAdditionalCosts

onSameLocation :: (HasCallStack, HasGame m) => InvestigatorId -> Placement -> m Bool
onSameLocation iid = \case
  AttachedToLocation lid -> fieldMap InvestigatorLocation (== Just lid) iid
  AtLocation lid -> fieldMap InvestigatorLocation (== Just lid) iid
  InVehicle aid -> do
    field AssetLocation aid >>= \case
      Nothing -> pure False
      Just lid -> fieldMap InvestigatorLocation (== Just lid) iid
  InPlayArea iid' ->
    if iid == iid'
      then pure True
      else do
        l1 <- join <$> fieldMay InvestigatorLocation iid
        l2 <- join <$> fieldMay InvestigatorLocation iid'
        pure $ isJust l1 && l1 == l2
  InThreatArea iid' ->
    if iid == iid'
      then pure True
      else do
        l1 <- join <$> fieldMay InvestigatorLocation iid
        l2 <- join <$> fieldMay InvestigatorLocation iid'
        pure $ isJust l1 && l1 == l2
  AttachedToEnemy eid ->
    liftA2 (==) (field EnemyLocation eid) (field InvestigatorLocation iid)
  AttachedToTreachery tid ->
    liftA2 (==) (field TreacheryLocation tid) (field InvestigatorLocation iid)
  AttachedToAsset aid _ -> do
    placement' <- field AssetPlacement aid
    onSameLocation iid placement'
  AttachedToAct _ -> pure False
  AttachedToAgenda _ -> pure False
  AttachedToInvestigator iid' ->
    liftA2
      (==)
      (field InvestigatorLocation iid')
      (field InvestigatorLocation iid)
  AsSwarm eid _ -> onSameLocation iid =<< field EnemyPlacement eid
  Unplaced -> pure False
  Global -> pure True
  Limbo -> pure False
  OutOfPlay _ -> pure False
  StillInHand _ -> pure False
  StillInDiscard _ -> pure False
  StillInEncounterDiscard -> pure False
  HiddenInHand _ -> pure False
  OnTopOfDeck _ -> pure False
  NextToAgenda -> pure False
  Near _ -> pure False

passesCriteria
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Maybe (Card, CostStatus)
  -> Source
  -> Source
  -> [Window]
  -> Criterion
  -> m Bool
passesCriteria iid mcard source' requestor windows' = \case
  Criteria.CanEnterThisVehicle -> do
    mods <- getModifiers iid
    let invalidMatcher =
          let matchers = [matcher | CannotEnterVehicle matcher <- mods]
           in if null matchers then Nothing else Just (Matcher.oneOf matchers)
    case source of
      AssetSource aid -> do
        let
          go = do
            field InvestigatorPlacement iid >>= \case
              AtLocation lid -> do
                mlid' <- field AssetLocation aid
                pure $ Just lid == mlid'
              _ -> pure False
        case invalidMatcher of
          Nothing -> go
          Just matcher -> do
            invalid <- aid <=~> matcher
            if invalid then pure False else go
      _ -> error $ "Unhandled vehicle source: " <> show source
  Criteria.CanLeaveThisVehicle -> do
    case source of
      AssetSource aid -> do
        field InvestigatorPlacement iid >>= \case
          InVehicle aid' | aid == aid' -> pure True
          _ -> pure False
      _ -> error $ "Unhandled vehicle source: " <> show source
  Criteria.NotInEliminatedBearersThreatArea -> do
    case source of
      EnemySource eid -> do
        field EnemyBearer eid >>= \case
          Just iid' ->
            field EnemyPlacement eid >>= \case
              InThreatArea iid'' | iid' == iid'' -> iid' <=~> Matcher.UneliminatedInvestigator
              _ -> pure True
          _ -> error $ "No enemy bearer for enemy: " <> show eid
      _ -> error $ "Unhandled bearer source: " <> show source
  Criteria.InThisVehicle -> do
    case source of
      AssetSource aid -> do
        field InvestigatorPlacement iid >>= \case
          InVehicle aid' | aid == aid' -> pure True
          _ -> pure False
      _ -> error $ "Unhandled vehicle source: " <> show source
  Criteria.KeyIsSetAside key -> (elem key) <$> scenarioField ScenarioSetAsideKeys
  Criteria.UnrevealedKeyIsSetAside -> do
    let
      unrevealedKey = \case
        UnrevealedKey _ -> True
        _ -> False
    any unrevealedKey . setToList <$> scenarioField ScenarioSetAsideKeys
  Criteria.TabooCriteria tabooList cIf cElse -> do
    mtabooList <- field InvestigatorTaboo iid
    passesCriteria iid mcard source' requestor windows'
      $ if maybe False (>= tabooList) mtabooList then cIf else cElse
  Criteria.IfYouOweBiancaDieKatz -> do
    let
      isValid = \case
        (YouOweBiancaResources (Labeled _ iid') _) -> iid == iid'
        _ -> False
    any isValid <$> scenarioField ScenarioRemembered
  Criteria.OnlySources mtchr -> sourceMatches requestor mtchr
  Criteria.HasCustomization c -> do
    case mcard of
      (Just (PlayerCard card, _)) -> pure $ hasCustomization_ (cdCustomizations $ toCardDef card) card.customizations c
      _ -> case source' of
        EventSource aid -> do
          attrs <- getAttrs @Event aid
          pure $ hasCustomization attrs c
        _ -> error $ "Unhandled source: " <> show source' <> " " <> show mcard
  Criteria.ChosenCustomizationCardIsInPlay -> do
    case mcard of
      (Just (PlayerCard card, _)) -> do
        let customizations = pcCustomizations card
        let titles = [t | ChosenCard t <- concatMap snd (toList customizations)]
        selectAny
          $ Matcher.basic (Matcher.oneOf $ Matcher.CardWithTitle <$> titles)
          <> Matcher.InPlayAreaOf (Matcher.InvestigatorWithId iid)
      _ -> case source' of
        EventSource aid -> do
          customizations <- field EventCustomizations aid
          let titles = [t | ChosenCard t <- concatMap snd (toList customizations)]
          selectAny
            $ Matcher.basic (Matcher.oneOf $ Matcher.CardWithTitle <$> titles)
            <> Matcher.InPlayAreaOf (Matcher.InvestigatorWithId iid)
        _ -> error $ "Unhandled source: " <> show source' <> " " <> show mcard
  Criteria.HasTrueMagick -> do
    case source' of
      AssetSource trueMagick -> do
        attrs <- getAttrs @Asset trueMagick
        hand <- fieldMap InvestigatorHand (filterCards (card_ $ #asset <> #spell)) iid
        let
          replaceAssetId = const attrs.id
          replaceAssetIds = over biplate replaceAssetId
        let handEntities =
              map
                ( \c ->
                    overAttrs (\attrs' -> attrs {assetCardCode = assetCardCode attrs'})
                      $ createAsset c
                      $ unsafeFromCardId c.id
                )
                hand
        let handAbilities =
              map
                ( \ab -> case ab.source of
                    AssetSource aid ->
                      overCost replaceAssetIds
                        $ ab {abilitySource = proxy attrs (CardIdSource $ unsafeToCardId aid)}
                    _ -> error "wrong source"
                )
                (concatMap getAbilities handEntities)
        anyM (getCanPerformAbility iid windows') handAbilities
      _ -> error "wrong source"
  Criteria.HasCalculation c valueMatcher -> do
    value <- calculate c
    gameValueMatches value valueMatcher
  Criteria.HasRemainingBlessTokens -> (> 0) <$> getRemainingBlessTokens
  Criteria.HasRemainingCurseTokens -> (> 0) <$> getRemainingCurseTokens
  Criteria.HasNRemainingCurseTokens valueMatcher -> (`gameValueMatches` valueMatcher) =<< getRemainingCurseTokens
  Criteria.HasMoreBlessThanCurseTokens ->
    (>)
      <$> selectCount (Matcher.ChaosTokenFaceIs #bless)
      <*> selectCount (Matcher.ChaosTokenFaceIs #curse)
  Criteria.HasMoreCurseThanBlessTokens ->
    (>)
      <$> selectCount (Matcher.ChaosTokenFaceIs #curse)
      <*> selectCount (Matcher.ChaosTokenFaceIs #bless)
  Criteria.CanMoveTo matcher -> notNull <$> getCanMoveToMatchingLocations iid source matcher
  Criteria.CanMoveThis dir -> do
    case source of
      LocationSource lid -> do
        cosmos' <- getCosmos
        case Cosmos.findInCosmos lid cosmos' of
          Nothing -> pure False
          Just pos -> pure $ Cosmos.isEmpty $ Cosmos.viewCosmos (Cosmos.updatePosition pos dir) cosmos'
      _ -> error "Only works on locations"
  Criteria.ChaosTokenCountIs tokenMatcher valueMatcher -> do
    n <- selectCount tokenMatcher
    gameValueMatches n valueMatcher
  Criteria.HasHistory hType iMatcher historyMatcher -> do
    investigators <- select iMatcher
    histories <- traverse (getHistory hType) investigators
    anyM (historyMatches historyMatcher) histories
  Criteria.HasScenarioCount key valueMatcher -> do
    n <- scenarioCount key
    gameValueMatches n valueMatcher
  Criteria.HasCampaignCount key valueMatcher -> do
    n <- getRecordCount key
    gameValueMatches n valueMatcher
  Criteria.NotYetRecorded key -> do
    recorded <- getHasRecord key
    pure $ not recorded
  Criteria.HasRecord key -> getHasRecord key
  Criteria.DuringPhase phaseMatcher -> do
    p <- getPhase
    matchPhase p phaseMatcher
  Criteria.ActionCanBeUndone -> getActionCanBeUndone
  Criteria.EncounterDeckIsNotEmpty -> do
    deck <- scenarioField ScenarioEncounterDeck
    pure $ not $ null deck
  Criteria.EncounterDeckWith cardListMatcher -> do
    deck <- scenarioFieldMap ScenarioEncounterDeck (map toCard . unDeck)
    cardListMatches deck cardListMatcher
  Criteria.DoomCountIs valueMatcher -> do
    doomCount <- getDoomCount
    gameValueMatches doomCount valueMatcher
  Criteria.PlayerCountIs n -> (== n) <$> getPlayerCount
  Criteria.Negate restriction ->
    not <$> passesCriteria iid mcard source requestor windows' restriction
  Criteria.AllUndefeatedInvestigatorsResigned ->
    andM
      [ selectNone Matcher.UneliminatedInvestigator
      , selectAny Matcher.ResignedInvestigator -- at least one investigator should have resigned
      ]
  Criteria.EachUndefeatedInvestigator investigatorMatcher -> do
    uneliminated <- select Matcher.UneliminatedInvestigator
    if null uneliminated
      then pure False
      else (== uneliminated) <$> select investigatorMatcher
  Criteria.Never -> pure False
  Criteria.InYourHand -> do
    hand <-
      liftA2 (<>) (fieldMap InvestigatorHand (map toCardId) iid) (map toCardId <$> getAsIfInHandCards iid)
    case source of
      EventSource eid -> do
        mCardId <- fieldMay InHandEventCardId eid
        case mCardId of
          Nothing -> pure False
          Just cardId -> pure $ cardId `elem` hand
      SkillSource sid -> do
        mCardId <- fieldMay InHandSkillCardId sid
        case mCardId of
          Nothing -> pure False
          Just cardId -> pure $ cardId `elem` hand
      AssetSource aid -> do
        inPlay <- selectAny $ Matcher.AssetWithId aid
        if inPlay
          then pure False
          else do
            -- todo we should make a cleaner method for this
            cardId <- field InHandAssetCardId aid
            pure $ cardId `elem` hand
      TreacherySource tid -> elem tid <$> select (Matcher.treacheryInHandOf iid)
      _ -> error $ "source not handled for in your hand: " <> show source
  Criteria.InYourDiscard -> do
    discard <- fieldMap InvestigatorDiscard (map toCardId) iid
    case source of
      AssetSource aid -> do
        inPlay <- selectAny $ Matcher.AssetWithId aid
        if inPlay
          then pure False
          else do
            -- todo we should make a cleaner method for this
            fieldMap InDiscardAssetCardId (`elem` discard) aid
      SkillSource aid -> do
        inPlay <- selectAny $ Matcher.SkillWithId aid
        if inPlay
          then pure False
          else pure $ unsafeToCardId aid `elem` discard
      InvestigatorSource _ -> case mcard of
        Just (card, _) -> pure $ toCardId card `elem` discard
        _ -> error "No card available to check"
      _ -> error $ "source not handled for in your discard: " <> show source
  Criteria.InThreatAreaOf (Matcher.replaceYouMatcher iid -> who) -> do
    case source of
      TreacherySource tid ->
        elem tid <$> select (Matcher.TreacheryInThreatAreaOf who)
      StorySource sid -> do
        placement <- field StoryPlacement sid
        case placement of
          InThreatArea iid' -> elem iid' <$> select who
          _ -> pure False
      EventSource eid -> do
        placement <- field EventPlacement eid
        case placement of
          InThreatArea iid' -> elem iid' <$> select who
          _ -> pure False
      _ ->
        error
          $ "Can not check if "
          <> show source
          <> " is in players threat area"
  Criteria.NotSetup -> not <$> getInSetup
  Criteria.Self -> case source of
    InvestigatorSource iid' -> pure $ iid == iid'
    _ -> pure False
  Criteria.ValueIs val valueMatcher -> gameValueMatches val valueMatcher
  Criteria.UnderneathCardCount valueMatcher zone cardMatcher -> do
    let
      getCards = \case
        Criteria.UnderAgendaDeck -> scenarioField ScenarioCardsUnderAgendaDeck
        Criteria.UnderActDeck -> scenarioField ScenarioCardsUnderActDeck
        Criteria.UnderZones zs -> concatMapM getCards zs
    cardCount <- length . filter (`cardMatch` cardMatcher) <$> getCards zone
    gameValueMatches cardCount valueMatcher
  Criteria.SelfHasModifier modifier -> case source of
    InvestigatorSource iid' ->
      elem modifier <$> getModifiers (InvestigatorTarget iid')
    EnemySource iid' -> elem modifier <$> getModifiers (EnemyTarget iid')
    AssetSource aid' -> elem modifier <$> getModifiers aid'
    _ -> pure False
  Criteria.Here -> case source of
    LocationSource lid -> fieldP InvestigatorLocation (== Just lid) iid
    ProxySource (LocationSource lid) _ ->
      fieldP InvestigatorLocation (== Just lid) iid
    IndexedSource _ (LocationSource lid) ->
      fieldP InvestigatorLocation (== Just lid) iid
    _ -> pure False
  Criteria.HasSupply s -> fieldP InvestigatorSupplies (elem s) iid
  Criteria.ControlsThis ->
    let
      go = \case
        ProxySource (CardIdSource _) s -> go s
        IndexedSource _ s -> go s
        ProxySource s _ -> go s
        AssetSource aid ->
          elem aid
            <$> select (Matcher.AssetControlledBy $ Matcher.InvestigatorWithId iid)
        EventSource eid ->
          elem eid
            <$> select (Matcher.EventControlledBy $ Matcher.InvestigatorWithId iid)
        SkillSource sid ->
          elem sid
            <$> select (Matcher.SkillControlledBy $ Matcher.InvestigatorWithId iid)
        _ -> pure False
     in
      go source
  Criteria.OwnsThis ->
    let
      go = \case
        ProxySource (CardIdSource _) s -> go s
        IndexedSource _ s -> go s
        ProxySource s _ -> go s
        AssetSource aid ->
          elem aid
            <$> select (Matcher.AssetOwnedBy $ Matcher.InvestigatorWithId iid)
        EventSource eid ->
          elem eid
            <$> select (Matcher.EventOwnedBy $ Matcher.InvestigatorWithId iid)
        SkillSource sid ->
          elem sid
            <$> select (Matcher.SkillOwnedBy $ Matcher.InvestigatorWithId iid)
        EnemySource eid ->
          elem eid
            <$> select (Matcher.EnemyOwnedBy $ Matcher.InvestigatorWithId iid)
        _ -> pure False
     in
      go source
  Criteria.DuringSkillTest skillTestMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just skillTest -> skillTestMatches iid source skillTest skillTestMatcher
  Criteria.ChargesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryResources tid
    _ -> error "missing ChargesOnThis check"
  Criteria.ResourcesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryResources tid
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetResources aid
    LocationSource aid ->
      (`gameValueMatches` valueMatcher) =<< field LocationResources aid
    _ -> error $ "missing ResourcesOnThis check: " <> show source
  Criteria.ResourcesOnLocation locationMatcher valueMatcher -> do
    total <- getSum <$> selectAgg Sum LocationResources locationMatcher
    gameValueMatches total valueMatcher
  Criteria.CluesOnThis valueMatcher -> case source of
    LocationSource lid ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues lid
    ActSource aid -> (`gameValueMatches` valueMatcher) =<< field ActClues aid
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetClues aid
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryClues tid
    _ -> error "missing CluesOnThis check"
  Criteria.HorrorOnThis valueMatcher -> case source of
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetHorror aid
    _ -> error $ "missing HorrorOnThis check for " <> show source
  Criteria.DamageOnThis valueMatcher -> case source of
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetDamage aid
    _ -> error $ "missing DamageOnThis check for " <> show source
  Criteria.ScenarioDeckWithCard key -> notNull <$> getScenarioDeck key
  Criteria.Uncontrolled -> case source of
    AssetSource aid -> fieldP AssetController isNothing aid
    ProxySource (CardIdSource _) (AssetSource aid) -> fieldP AssetController isNothing aid
    ProxySource (AssetSource aid) _ -> fieldP AssetController isNothing aid
    IndexedSource _ (AssetSource aid) -> fieldP AssetController isNothing aid
    _ -> error $ "missing ControlsThis check for source: " <> show source
  Criteria.OnSameLocation -> do
    ignored <- hasModifier iid IgnoreOnSameLocation
    if ignored
      then pure True
      else do
        let
          go = \case
            StorySource sid -> onSameLocation iid =<< field StoryPlacement sid
            AssetSource aid -> onSameLocation iid =<< field AssetPlacement aid
            EnemySource eid -> onSameLocation iid =<< field EnemyPlacement eid
            TreacherySource tid -> onSameLocation iid =<< field TreacheryPlacement tid
            ProxySource (CardIdSource _) (AssetSource aid) -> go (AssetSource aid)
            ProxySource inner _ -> go inner
            IndexedSource _ inner -> go inner
            _ -> error $ "missing OnSameLocation check for source: " <> show source
        go source
  Criteria.DuringTurn (Matcher.replaceYouMatcher iid -> who) -> selectAny (Matcher.TurnInvestigator <> who)
  Criteria.CardExists cardMatcher -> selectAny cardMatcher
  Criteria.ExtendedCardExists cardMatcher ->
    case mcard of
      Just (card, _) -> selectAny (Matcher.replaceThisCard (toCardId card) cardMatcher)
      _ -> selectAny cardMatcher
  Criteria.CommitedCardsMatch cardListMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just st -> cardListMatches (concat $ toList (skillTestCommittedCards st)) cardListMatcher
  Criteria.PlayableCardExistsWithCostReduction reduction cardMatcher -> do
    mTurnInvestigator <- selectOne Matcher.TurnInvestigator
    let n = case reduction of
          Reduce x -> x
          ReduceBySuccessAmount -> getPassedBy windows'
    let
      updatedWindows = case mTurnInvestigator of
        Nothing -> windows'
        Just tIid ->
          nub $ mkWhen (Window.DuringTurn tIid) : windows'
    availableResources <- getSpendableResources iid
    results <- select cardMatcher
    -- GameSource is important because it allows us to skip the action cost
    anyM
      ( getIsPlayableWithResources
          iid
          GameSource
          (availableResources + n)
          (UnpaidCost NoAction)
          updatedWindows
      )
      results
  Criteria.PlayableCardExists costStatus cardMatcher -> do
    mTurnInvestigator <- selectOne Matcher.TurnInvestigator
    let
      updatedWindows = case mTurnInvestigator of
        Nothing -> windows'
        Just tIid ->
          nub $ mkWhen (Window.DuringTurn tIid) : windows'
    results <- select cardMatcher
    anyM (getIsPlayable iid source' costStatus updatedWindows) results
  Criteria.PlayableCardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
      windows'' =
        [ mkWhen (Window.DuringTurn iid)
        , mkWhen Window.FastPlayerWindow
        ]
    investigatorIds <-
      filterM
        ( fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers
            . InvestigatorTarget
        )
        =<< select investigatorMatcher
    discards <-
      filter (`cardMatch` cardMatcher)
        <$> concatMapM (field InvestigatorDiscard) investigatorIds
    anyM (getIsPlayable iid source (UnpaidCost NoAction) windows'' . PlayerCard) discards
  Criteria.FirstAction -> fieldP InvestigatorActionsTaken null iid
  Criteria.NoRestriction -> pure True
  Criteria.OnLocation locationMatcher -> do
    ignored <- hasModifier iid IgnoreOnSameLocation
    if ignored
      then pure True
      else do
        mlid <- field InvestigatorLocation iid
        case mlid of
          Nothing -> pure False
          Just lid ->
            anyM
              (\window -> locationMatches iid source window lid locationMatcher)
              windows'
  Criteria.ReturnableCardInDiscard discardSignifier traits -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- select (investigatorMatcher <> can.have.cards.leaveDiscard)
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  Criteria.CanAffordCostIncrease n -> do
    let
      go :: HasGame n => Maybe (Card, CostStatus) -> n Bool
      go = \case
        Just (card, AuxiliaryCost aux inner) -> do
          withModifiers
            card
            (modified GameSource [IncreaseCostOf (Matcher.CardWithId card.id) $ totalResourceCost aux])
            $ go (Just (card, inner))
        Just (card, UnpaidCost _) -> do
          cost <- getModifiedCardCost iid card
          resources <- getSpendableResources iid
          pure $ resources >= cost + n
        Just (_, PaidCost) -> pure True
        Nothing -> error $ "no card for CanAffordCostIncrease: " <> show source
    go mcard
  Criteria.CardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- select investigatorMatcher
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let filteredDiscards = filter (`cardMatch` cardMatcher) discards
    pure $ notNull filteredDiscards
  Criteria.ClueOnLocation ->
    maybe (pure False) (fmap (maybe False (> 0)) . fieldMay LocationClues)
      =<< field InvestigatorLocation iid
  Criteria.EnemyCriteria enemyCriteria ->
    passesEnemyCriteria iid source windows' enemyCriteria
  Criteria.SetAsideCardExists matcher -> selectAny (Matcher.SetAsideCardMatch matcher)
  Criteria.OutOfPlayEnemyExists outOfPlayZone matcher ->
    selectAny $ Matcher.OutOfPlayEnemy outOfPlayZone matcher
  Criteria.OnAct step -> do
    actId <- selectJust Matcher.AnyAct
    (== AS.ActStep step) . AS.actStep <$> field ActSequence actId
  Criteria.AgendaExists matcher -> selectAny matcher
  Criteria.AbilityExists matcher -> selectAny matcher
  Criteria.SkillExists matcher -> selectAny matcher
  Criteria.StoryExists matcher -> selectAny matcher
  Criteria.ActExists matcher -> selectAny matcher
  Criteria.CardWithDoomExists -> do
    orM
      [ selectAny $ Matcher.AssetWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.InvestigatorWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.EnemyWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.EventWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.LocationWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.TreacheryWithDoom (Matcher.atLeast 1)
      , selectAny $ Matcher.AgendaWithDoom (Matcher.atLeast 1)
      ]
  Criteria.AssetExists matcher -> do
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.TargetExists matcher -> do
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.DifferentAssetsExist matcher1 matcher2 -> do
    m1 <- select (Matcher.replaceYouMatcher iid matcher1)
    m2 <- select (Matcher.replaceYouMatcher iid matcher2)
    case (m1, m2) of
      ([], _) -> pure False
      (_, []) -> pure False
      ([x], [y]) -> pure $ x /= y
      _ -> pure True
  Criteria.DifferentEnemiesExist matcher1 matcher2 -> do
    m1 <- select (Matcher.replaceYouMatcher iid matcher1)
    m2 <- select (Matcher.replaceYouMatcher iid matcher2)
    case (m1, m2) of
      ([], _) -> pure False
      (_, []) -> pure False
      ([x], [y]) -> pure $ x /= y
      _ -> pure True
  Criteria.EventExists matcher -> do
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.EventWindowInvestigatorIs whoMatcher -> do
    windows'' :: [[Window]] <- drop 1 <$> getWindowStack
    case windows'' of
      ((windowType -> x) : _) : _ -> case x of
        Window.DrawCard iid' _ _ -> iid' <=~> Matcher.replaceYouMatcher iid whoMatcher
        _ -> pure False
      _ -> pure False
  Criteria.ExcludeWindowAssetExists matcher -> case getWindowAsset windows' of
    Nothing -> pure False
    Just aid -> do
      selectAny
        $ Matcher.NotAsset (Matcher.AssetWithId aid)
        <> Matcher.replaceYouMatcher iid matcher
  Criteria.TreacheryExists matcher -> selectAny matcher
  Criteria.InvestigatorExists matcher ->
    -- Because the matcher can't tell who is asking, we need to replace
    -- The You matcher by the Id of the investigator asking
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.InvestigatorsHaveSpendableClues valueMatcher -> do
    total <-
      getSum
        <$> selectAgg
          Sum
          InvestigatorClues
          (Matcher.InvestigatorWithoutModifier CannotSpendClues)
    total `gameValueMatches` valueMatcher
  Criteria.Criteria rs -> allM (passesCriteria iid mcard source' requestor windows') rs
  Criteria.AnyCriterion rs -> anyM (passesCriteria iid mcard source' requestor windows') rs
  Criteria.LocationExists matcher -> do
    selectAny (Matcher.replaceYouMatcher iid matcher)
  Criteria.LocationCount n matcher -> do
    (== n) <$> selectCount (Matcher.replaceYouMatcher iid matcher)
  Criteria.AssetCount n matcher -> do
    (== n) <$> selectCount (Matcher.replaceYouMatcher iid matcher)
  Criteria.EventCount valueMatcher matcher -> do
    n <- selectCount (Matcher.replaceYouMatcher iid matcher)
    gameValueMatches n valueMatcher
  Criteria.ExtendedCardCount n matcher ->
    (== n) <$> selectCount matcher
  Criteria.AllLocationsMatch targetMatcher locationMatcher -> do
    targets <- select (Matcher.replaceYouMatcher iid targetMatcher)
    actual <- select (Matcher.replaceYouMatcher iid locationMatcher)
    pure $ all (`elem` actual) targets
  Criteria.InvestigatorIsAlone ->
    (== 1) <$> selectCount (Matcher.colocatedWith iid)
  Criteria.InVictoryDisplay cardMatcher valueMatcher -> do
    vCards <- filter (`cardMatch` cardMatcher) <$> getVictoryDisplay
    gameValueMatches (length vCards) valueMatcher
  Criteria.OwnCardWithDoom -> do
    anyAssetsHaveDoom <-
      selectAny
        (Matcher.AssetControlledBy Matcher.You <> Matcher.AssetWithAnyDoom)
    investigatorHasDoom <- fieldP InvestigatorDoom (> 0) iid
    pure $ investigatorHasDoom || anyAssetsHaveDoom
  Criteria.ScenarioCardHasResignAbility -> do
    actions' <- getAllAbilities
    pure $ flip
      any
      actions'
      \ability -> case abilityType ability of
        ActionAbility [Action.Resign] _ -> True
        _ -> False
  Criteria.Remembered logKey -> do
    elem logKey <$> scenarioFieldMap ScenarioRemembered Set.toList
  Criteria.RememberedAtLeast value logKeys -> do
    n <-
      length
        . filter (`elem` logKeys)
        <$> scenarioFieldMap ScenarioRemembered Set.toList
    gameValueMatches n (Matcher.AtLeast value)
  Criteria.AtLeastNCriteriaMet n criteria -> do
    m <- countM (passesCriteria iid mcard source requestor windows') criteria
    pure $ m >= n
  Criteria.DuringAction -> case mcard of
    Just (_, PaidCost) -> pure False -- If the cost is paid we're in a play action so we have to assume it is always False or it will never trigger
    _ -> getGameInAction
  Criteria.AffectedByTarot -> case source of
    TarotSource card -> affectedByTarot iid card
    _ -> pure False
 where
  source = case source' of
    AbilitySource s _ -> s
    _ -> source'

getWindowAsset :: [Window] -> Maybe AssetId
getWindowAsset [] = Nothing
getWindowAsset ((windowType -> Window.ActivateAbility _ _ ability) : xs) = case abilitySource ability of
  AssetSource aid -> Just aid
  _ -> getWindowAsset xs
getWindowAsset (_ : xs) = getWindowAsset xs

-- | Build a matcher and check the list
passesEnemyCriteria
  :: HasGame m
  => InvestigatorId
  -> Source
  -> [Window]
  -> Criteria.EnemyCriterion
  -> m Bool
passesEnemyCriteria iid source windows' criterion = do
  bountiesOnly <- hasModifier iid BountiesOnly
  let matcherF = if bountiesOnly then (Matcher.EnemyWithBounty <>) else id
  selectAny . matcherF . Matcher.replaceYouMatcher iid =<< matcher criterion
 where
  matcher = \case
    Criteria.EnemyMatchesCriteria ms -> mconcatMapM matcher ms
    Criteria.EnemyExists m -> pure m
    Criteria.EnemyExistsAtAttachedLocation m -> case source of
      EventSource e -> do
        fieldMap EventPlacement placementToAttached e >>= \case
          Just (LocationTarget lid) ->
            pure $ m <> Matcher.EnemyAt (Matcher.LocationWithId lid)
          Just _ -> error "Event must be attached to a location"
          Nothing -> error "Event must be attached to a location"
      _ -> error $ "Does not handle source: " <> show source
    Criteria.ThisEnemy enemyMatcher -> case source of
      EnemySource eid -> pure $ Matcher.EnemyWithId eid <> enemyMatcher
      _ -> error "Invalid source for ThisEnemy"
    Criteria.NotAttackingEnemy ->
      -- TODO: should not be multiple enemies, but if so need to OR not AND matcher
      let
        getAttackingEnemy = \case
          Window _ (Window.EnemyAttacks details) _ -> Just $ attackEnemy details
          _ -> Nothing
       in
        case mapMaybe getAttackingEnemy windows' of
          [] -> error "can not be called without enemy source"
          xs -> pure $ Matcher.NotEnemy (concatMap Matcher.EnemyWithId xs)

getPotentiallyModifiedCardCost
  :: HasGame m => InvestigatorId -> Card -> Bool -> Int -> m Int
getPotentiallyModifiedCardCost iid c@(PlayerCard _) excludeChuckFergus startingCost = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  cardModifiers <- getModifiers (CardIdTarget $ toCardId c)
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  applyModifier n (CanReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (ChuckFergus2Modifier cardMatcher m) | not excludeChuckFergus = do
    -- get is playable will check if this has to be used, will likely break if
    -- anything else aside from Chuck Fergus (2) interacts with this
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n _ = pure n
getPotentiallyModifiedCardCost iid c@(EncounterCard _) excludeChuckFergus _ = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (CanReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (ChuckFergus2Modifier cardMatcher m) | not excludeChuckFergus = do
    -- get is playable will check if this has to be used
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n _ = pure n
getPotentiallyModifiedCardCost _ (VengeanceCard _) _ _ =
  error "should not check vengeance card"

cardInFastWindows
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Card
  -> [Window]
  -> Matcher.WindowMatcher
  -> m Bool
cardInFastWindows iid source card windows' matcher =
  anyM (\window -> windowMatches iid source' window matcher) windows'
 where
  source' = case card of
    PlayerCard pc -> BothSource source (CardIdSource pc.id)
    _ -> source

windowMatches
  :: (HasGame m, HasCallStack)
  => InvestigatorId
  -> Source
  -> Window
  -> Matcher.WindowMatcher
  -> m Bool
windowMatches _ _ (windowType -> Window.DoNotCheckWindow) _ = pure True
windowMatches iid rawSource window'@(windowTiming &&& windowType -> (timing', wType)) umtchr = do
  (source, mcard) <-
    case rawSource of
      BothSource s (CardIdSource cid) -> do
        card <- getCard cid
        pure (s, Just (card, UnpaidCost NeedsAction))
      _ -> pure (rawSource, Nothing)

  let noMatch = pure False
  let isMatch = pure True
  let guardTiming t body = if timing' == t then body wType else noMatch
  let mtchr = Matcher.replaceYouMatcher iid umtchr
  case mtchr of
    Matcher.TakeControlOfClues timing whoMatcher sourceMatcher -> guardTiming timing \case
      Window.TakeControlOfClues who source' -> do
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.VehicleEnters timing assetMatcher whereMatcher -> guardTiming timing \case
      Window.VehicleEnters aid where' -> do
        andM
          [ locationMatches iid source window' where' whereMatcher
          , aid <=~> assetMatcher
          ]
      _ -> noMatch
    Matcher.VehicleLeaves timing assetMatcher whereMatcher -> guardTiming timing \case
      Window.VehicleLeaves aid where' -> do
        andM
          [ locationMatches iid source window' where' whereMatcher
          , aid <=~> assetMatcher
          ]
      _ -> noMatch
    Matcher.WouldPlaceClueOnLocation timing whoMatcher whereMatcher valueMatcher -> guardTiming timing \case
      Window.WouldPlaceClueOnLocation who where' _ n -> do
        andM
          [ matchWho iid who whoMatcher
          , locationMatches iid source window' where' whereMatcher
          , gameValueMatches n valueMatcher
          ]
      _ -> noMatch
    Matcher.WouldAddChaosTokensToChaosBag timing mWhoMatcher valueMatcher face -> guardTiming timing \case
      Window.WouldAddChaosTokensToChaosBag mWho tokens -> do
        let matchCount = count (== face) tokens
        andM
          [ gameValueMatches matchCount valueMatcher
          , maybe
              (pure True)
              (\whoMatcher -> maybe (pure False) (\who -> matchWho iid who whoMatcher) mWho)
              mWhoMatcher
          ]
      _ -> noMatch
    Matcher.RevealChaosTokensDuringSkillTest timing whoMatcher skillTestMatcher chaosTokenMatcher -> guardTiming timing \case
      Window.RevealChaosTokensDuringSkillTest who st chaosTokens -> do
        andM
          [ matchWho iid who whoMatcher
          , skillTestMatches iid source st skillTestMatcher
          , anyM (`chaosTokenMatches` Matcher.IncludeSealed chaosTokenMatcher) chaosTokens
          ]
      _ -> noMatch
    Matcher.InvestigatorPlacedFromTheirPool timing whoMatcher sourceMatcher targetMatcher tType -> guardTiming timing \case
      Window.InvestigatorPlacedFromTheirPool who source' target' tType' _ | tType == tType' -> do
        andM
          [ matchWho iid who whoMatcher
          , target' `targetMatches` targetMatcher
          , source' `sourceMatches` sourceMatcher
          ]
      _ -> noMatch
    Matcher.AttachCard timing mWhoMatcher cardMatcher targetMatcher -> guardTiming timing \case
      Window.AttachCard mWho card target -> do
        andM
          [ pure $ card `cardMatch` cardMatcher
          , maybe
              (pure True)
              (\matcher -> maybe (pure False) (\who -> matchWho iid who matcher) mWho)
              mWhoMatcher
          , target `targetMatches` targetMatcher
          ]
      _ -> noMatch
    Matcher.WindowWhen criteria mtchr' -> do
      (&&)
        <$> passesCriteria iid mcard source source [window'] criteria
        <*> windowMatches iid rawSource window' mtchr'
    Matcher.NotAnyWindow -> noMatch
    Matcher.AnyWindow -> isMatch
    Matcher.FloodLevelChanged timing whereMatcher -> guardTiming timing \case
      Window.FloodLevelChanged where' _ _ -> locationMatches iid source window' where' whereMatcher
      _ -> noMatch
    Matcher.FloodLevelIncreased timing whereMatcher -> guardTiming timing \case
      Window.FloodLevelChanged where' fl1 fl2 | fl2 > fl1 -> locationMatches iid source window' where' whereMatcher
      _ -> noMatch
    Matcher.FirstTimeParleyingThisRound timing whoMatcher -> guardTiming timing \case
      Window.FirstTimeParleyingThisRound who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.ScenarioCountIncremented timing k -> guardTiming timing \case
      Window.ScenarioCountIncremented k' -> pure $ k == k'
      _ -> noMatch
    Matcher.SkillTestStep timing step -> guardTiming timing \case
      Window.SkillTestStep step' -> pure $ step == step'
      _ -> noMatch
    Matcher.PlacedToken timing sourceMatcher targetMatcher token -> guardTiming timing \case
      Window.PlacedToken source' target' token' _ ->
        andM
          [ pure $ token == token'
          , targetMatches target' targetMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.EntersThreatArea timing whoMatcher cardMatcher -> guardTiming timing \case
      Window.EntersThreatArea who card -> do
        andM
          [ pure $ card `cardMatch` cardMatcher
          , matchWho iid who whoMatcher
          ]
      _ -> noMatch
    Matcher.WouldPayCardCost timing whomatcher cardMatcher -> guardTiming timing \case
      Window.WouldPayCardCost who _ _ card -> do
        andM
          [ pure $ card `cardMatch` cardMatcher
          , matchWho iid who whomatcher
          ]
      _ -> noMatch
    Matcher.AttackOrEffectSpentLastUse timing sourceMatcher targetMatcher uType -> guardTiming timing $ \case
      Window.AttackOrEffectSpentLastUse source' target' uType' | uType == uType' -> do
        andM
          [ sourceMatches source' sourceMatcher
          , targetMatches target' targetMatcher
          ]
      _ -> noMatch
    Matcher.SpentUses timing whoMatcher sourceMatcher uType assetMatcher valueMatcher -> guardTiming timing $ \case
      Window.SpentUses who source' assetId uType' n | uType == uType' -> do
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          , assetId <=~> assetMatcher
          , gameValueMatches n valueMatcher
          ]
      _ -> noMatch
    Matcher.WouldSearchDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.WouldSearchDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch who deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.SearchedDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.SearchedDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch iid deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.WouldLookAtDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.WouldLookAtDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch who deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.LookedAtDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.LookedAtDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch iid deck
              $ Matcher.replaceThatInvestigator who deckMatcher
          ]
      _ -> noMatch
    Matcher.TokensWouldBeRemovedFromChaosBag timing matcher -> guardTiming timing $ \case
      Window.TokensWouldBeRemovedFromChaosBag tokens' -> anyM (`chaosTokenMatches` Matcher.InTokenPool matcher) tokens'
      _ -> noMatch
    Matcher.WouldTriggerChaosTokenRevealEffectOnCard whoMatcher cardMatcher tokens ->
      guardTiming Timing.AtIf $ \case
        Window.RevealChaosTokenEffect who token effectId -> do
          cardCode <- field EffectCardCode effectId
          andM
            [ pure $ chaosTokenFace token `elem` tokens
            , pure $ lookupCard cardCode nullCardId `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        Window.RevealChaosTokenTreacheryEffect who tokens' treacheryId -> do
          card <- field TreacheryCard treacheryId
          andM
            [ pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        Window.RevealChaosTokenEventEffect who tokens' eventId -> do
          card <- field EventCard eventId
          andM
            [ pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        Window.RevealChaosTokenAssetAbilityEffect who tokens' assetId -> do
          card <- field AssetCard assetId
          andM
            [ pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.GameBegins timing -> guardTiming timing $ pure . (== Window.GameBegins)
    Matcher.InvestigatorTakeDamage timing whoMatcher sourceMatcher ->
      guardTiming timing \case
        Window.TakeDamage source' _ (InvestigatorTarget who) _ ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        Window.DealtDamage source' _ (InvestigatorTarget who) _ ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorTakeHorror timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeHorror source' (InvestigatorTarget who) ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.DealtHorror source' (InvestigatorTarget who) _ ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldTakeDamage timing whoMatcher sourceMatcher damageTypeMatcher ->
      guardTiming timing $ \case
        Window.WouldTakeDamage source' (InvestigatorTarget who) _ strategy ->
          andM
            [ pure $ damageTypeMatches strategy damageTypeMatcher
            , matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.WouldTakeDamageOrHorror source' (InvestigatorTarget who) n _ | n > 0 -> do
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldTakeHorror timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.WouldTakeHorror source' (InvestigatorTarget who) _ ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.WouldTakeDamageOrHorror source' (InvestigatorTarget who) _ n | n > 0 -> do
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.SuccessfullyInvestigatedWithNoClues timing whoMatcher whereMatcher -> guardTiming timing $ \case
      Window.SuccessfullyInvestigateWithNoClues who where' -> do
        andM
          [ matchWho iid who whoMatcher
          , locationMatches iid source window' where' whereMatcher
          ]
      _ -> pure False
    Matcher.LostActions timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.LostActions who source' _ ->
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.ScenarioEvent timing eKey -> guardTiming timing \case
      Window.ScenarioEvent eKey' _ -> pure $ eKey == eKey'
      _ -> noMatch
    Matcher.LostResources timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.LostResources who source' _ ->
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.CancelledOrIgnoredCardOrGameEffect sourceMatcher ->
      guardTiming Timing.After $ \case
        Window.CancelledOrIgnoredCardOrGameEffect source' ->
          sourceMatches source' sourceMatcher
        _ -> noMatch
    Matcher.WouldBeShuffledIntoDeck deckMatcher cardMatcher -> case wType of
      Window.WouldBeShuffledIntoDeck deck card
        | cardMatch card cardMatcher ->
            deckMatch iid deck deckMatcher
      _ -> noMatch
    Matcher.AddingToCurrentDepth -> case wType of
      Window.AddingToCurrentDepth -> isMatch
      _ -> noMatch
    Matcher.DrawingStartingHand timing whoMatcher -> guardTiming timing $ \case
      Window.DrawingStartingHand who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.MovedFromHunter timing enemyMatcher -> guardTiming timing $ \case
      Window.MovedFromHunter eid -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.EnemyMovedTo timing locationMatcher movesVia enemyMatcher -> guardTiming timing $ \case
      Window.EnemyMovesTo lid movesVia' eid
        | movesVia == Matcher.MovedViaAny || movesVia == movesVia' ->
            andM [elem eid <$> select enemyMatcher, elem lid <$> select locationMatcher]
      _ -> noMatch
    Matcher.PlaceUnderneath timing targetMatcher cardMatcher -> guardTiming timing $ \case
      Window.PlaceUnderneath target' card ->
        andM
          [ pure $ cardMatch card cardMatcher
          , targetMatches target' targetMatcher
          ]
      _ -> noMatch
    Matcher.ActivateAbility timing whoMatcher abilityMatcher -> guardTiming timing $ \case
      Window.ActivateAbility who _ ability ->
        andM
          [ matchWho iid who whoMatcher
          , elem ability <$> select abilityMatcher
          ]
      _ -> noMatch
    Matcher.CommittedCard timing whoMatcher cardMatcher -> guardTiming timing $ \case
      Window.CommittedCard who card ->
        andM
          [ pure $ cardMatch card cardMatcher
          , matchWho iid who whoMatcher
          ]
      _ -> noMatch
    Matcher.CommittedCards timing whoMatcher cardListMatcher -> guardTiming timing $ \case
      Window.CommittedCards who cards ->
        andM
          [ matchWho iid who whoMatcher
          , cardListMatches cards cardListMatcher
          ]
      _ -> noMatch
    Matcher.EnemyWouldSpawnAt enemyMatcher locationMatcher ->
      case wType of
        Window.EnemyWouldSpawnAt eid lid -> do
          andM
            [ enemyMatches eid enemyMatcher
            , lid <=~> locationMatcher
            ]
        _ -> noMatch
    Matcher.EnemyAttemptsToSpawnAt timing enemyMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttemptsToSpawnAt eid locationMatcher' -> do
          case locationMatcher of
            Matcher.LocationNotInPlay -> do
              andM
                [ enemyMatches eid enemyMatcher
                , selectNone locationMatcher'
                ]
            other | other == locationMatcher' -> enemyMatches eid enemyMatcher
            _ -> noMatch -- TODO: We may need more things here
        _ -> noMatch
    Matcher.TookControlOfAsset timing whoMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.TookControlOfAsset who aid ->
          andM
            [ matchWho iid who whoMatcher
            , elem aid <$> select assetMatcher
            ]
        _ -> noMatch
    Matcher.AssetHealed timing damageType assetMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.Healed damageType' (AssetTarget assetId) source' _ | damageType == damageType' -> do
          andM
            [ elem assetId <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorHealed timing damageType whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.Healed damageType' (InvestigatorTarget who) source' _ | damageType == damageType' -> do
          andM
            [matchWho iid who whoMatcher, sourceMatches source' sourceMatcher]
        _ -> noMatch
    Matcher.WouldPerformRevelationSkillTest timing whoMatcher ->
      guardTiming timing $ \case
        Window.WouldPerformRevelationSkillTest who _ -> matchWho iid who whoMatcher
        _ -> noMatch
    Matcher.WouldDrawEncounterCard timing whoMatcher phaseMatcher ->
      guardTiming timing $ \case
        Window.WouldDrawEncounterCard who p ->
          andM [matchWho iid who whoMatcher, matchPhase p phaseMatcher]
        _ -> noMatch
    Matcher.AmongSearchedCards whoMatcher -> case wType of
      Window.AmongSearchedCards _ who -> do
        field InvestigatorSearch who >>= \case
          Nothing -> pure False
          Just search' ->
            andM
              [ maybe False (`elem` search'.allFoundCards) <$> sourceToMaybeCard source
              , matchWho iid who whoMatcher
              ]
      _ -> noMatch
    Matcher.WouldDiscardFromHand timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.WouldDiscardFromHand who source' ->
          andM
            [ matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.Discarded timing mWhoMatcher sourceMatcher cardMatcher ->
      guardTiming timing $ \case
        Window.Discarded mWho source' card ->
          andM
            [ maybe
                (pure True)
                (\matcher -> maybe (pure False) (\who -> matchWho iid who matcher) mWho)
                mWhoMatcher
            , sourceMatches source' sourceMatcher
            , extendedCardMatch card cardMatcher
            ]
        _ -> noMatch
    Matcher.AssetWouldBeDiscarded timing assetMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.EventWouldBeDiscarded timing eventMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (EventTarget aid) -> elem aid <$> select eventMatcher
      _ -> noMatch
    Matcher.EnemyWouldBeDiscarded timing enemyMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (EnemyTarget eid) -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.EnemyDiscarded timing sourceMatcher enemyMatcher -> guardTiming timing $ \case
      Window.EntityDiscarded source' (EnemyTarget eid) ->
        andM
          [ eid <=~> enemyMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.TreacheryWouldBeDiscarded timing treacheryMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (TreacheryTarget tid) -> elem tid <$> select treacheryMatcher
      _ -> noMatch
    Matcher.TreacheryDiscarded timing sourceMatcher treacheryMatcher -> guardTiming timing $ \case
      Window.EntityDiscarded source' (TreacheryTarget tid) ->
        andM
          [ tid <=~> treacheryMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.AgendaAdvances timing agendaMatcher -> guardTiming timing $ \case
      Window.AgendaAdvance aid -> agendaMatches aid agendaMatcher
      _ -> noMatch
    Matcher.ActAdvances timing actMatcher -> guardTiming timing $ \case
      Window.ActAdvance aid -> actMatches aid actMatcher
      _ -> noMatch
    Matcher.Exhausts timing whoMatcher targetMatcher -> guardTiming timing \case
      Window.Exhausts target@(AssetTarget aid) -> do
        mController <- field AssetController aid
        case mController of
          Just controller -> do
            andM
              [ matchWho iid controller whoMatcher
              , targetMatches target targetMatcher
              ]
          Nothing -> noMatch
      _ -> noMatch
    Matcher.EnemyExhausts timing enemyMatcher -> guardTiming timing \case
      Window.Exhausts (EnemyTarget eid) -> enemyMatches eid enemyMatcher
      _ -> noMatch
    Matcher.MovedBy timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.Moves who source' _ _ ->
        andM
          [ matchWho iid who whoMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.MovedButBeforeEnemyEngagement timing whoMatcher whereMatcher ->
      guardTiming timing $ \case
        Window.MovedButBeforeEnemyEngagement who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId whereMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorDefeated timing defeatedByMatcher whoMatcher ->
      guardTiming timing $ \case
        Window.InvestigatorDefeated defeatedBy who ->
          andM
            [ matchWho iid who whoMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldBeDefeated timing defeatedByMatcher whoMatcher ->
      guardTiming timing $ \case
        Window.InvestigatorWouldBeDefeated defeatedBy who ->
          andM
            [ matchWho iid who whoMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.AgendaWouldAdvance timing advancementReason agendaMatcher ->
      guardTiming timing $ \case
        Window.AgendaWouldAdvance advancementReason' aid | advancementReason == advancementReason' -> do
          agendaMatches aid agendaMatcher
        _ -> noMatch
    Matcher.WouldPlaceDoomCounter timing sourceMatcher targetMatcher -> guardTiming timing $ \case
      Window.WouldPlaceDoom source' target _ ->
        andM [targetMatches target targetMatcher, sourceMatches source' sourceMatcher]
      _ -> noMatch
    Matcher.PlacedDoomCounter timing sourceMatcher targetMatcher -> guardTiming timing $ \case
      Window.PlacedDoom source' target _ ->
        andM [targetMatches target targetMatcher, sourceMatches source' sourceMatcher]
      _ -> noMatch
    Matcher.PlacedDoomCounterOnTargetWithNoDoom timing sourceMatcher targetMatcher -> guardTiming timing $ \case
      Window.PlacedDoomCounterOnTargetWithNoDoom source' target _ ->
        andM [targetMatches target targetMatcher, sourceMatches source' sourceMatcher]
      _ -> noMatch
    Matcher.WouldPlaceBreach timing targetMatcher -> guardTiming timing $ \case
      Window.WouldPlaceBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.PlacedBreaches timing targetMatcher -> guardTiming timing $ \case
      Window.PlacedBreaches target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.PlacedBreach timing targetMatcher -> guardTiming timing $ \case
      Window.PlacedBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.WouldRemoveBreach timing targetMatcher -> guardTiming timing $ \case
      Window.WouldRemoveBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.RemovedBreaches timing targetMatcher -> guardTiming timing $ \case
      Window.RemovedBreaches target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.RemovedBreach timing targetMatcher -> guardTiming timing $ \case
      Window.RemovedBreach target -> targetMatches target targetMatcher
      _ -> noMatch
    Matcher.PlacedCounter timing whoMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedHorror source' (InvestigatorTarget iid') n | counterMatcher == Matcher.HorrorCounter -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDamage source' (InvestigatorTarget iid') n | counterMatcher == Matcher.DamageCounter -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnLocation timing whereMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedClues source' (LocationTarget locationId) n | counterMatcher == Matcher.ClueCounter -> do
          andM
            [ locationMatches iid source window' locationId whereMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedResources source' (LocationTarget locationId) n | counterMatcher == Matcher.ResourceCounter -> do
          andM
            [ locationMatches iid source window' locationId whereMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDamage source' (LocationTarget locationId) n | counterMatcher == Matcher.DamageCounter -> do
          andM
            [ locationMatches iid source window' locationId whereMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnEnemy timing enemyMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedClues source' (EnemyTarget enemyId) n | counterMatcher == Matcher.ClueCounter -> do
          andM
            [ enemyMatches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDoom source' (EnemyTarget enemyId) n | counterMatcher == Matcher.DoomCounter -> do
          andM
            [ enemyMatches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnAgenda timing agendaMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedDoom source' (AgendaTarget agendaId) n | counterMatcher == Matcher.DoomCounter -> do
          andM
            [ agendaMatches agendaId agendaMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.PlacedCounterOnAsset timing assetMatcher sourceMatcher counterMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.PlacedHorror source' (AssetTarget assetId) n | counterMatcher == Matcher.HorrorCounter -> do
          andM
            [ assetId <=~> assetMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        Window.PlacedDamage source' (AssetTarget assetId) n | counterMatcher == Matcher.DamageCounter -> do
          andM
            [ assetId <=~> assetMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.RevealLocation timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.RevealLocation who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.FlipLocation timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.FlipLocation who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.GameEnds timing -> guardTiming timing (pure . (== Window.EndOfGame))
    Matcher.InvestigatorEliminated timing whoMatcher -> guardTiming timing $ \case
      Window.InvestigatorEliminated who ->
        matchWho iid who (Matcher.IncludeEliminated $ Matcher.replaceYouMatcher iid whoMatcher)
      _ -> noMatch
    Matcher.InvestigatorResigned timing whoMatcher -> guardTiming timing $ \case
      Window.InvestigatorResigned who ->
        matchWho iid who (Matcher.IncludeEliminated $ Matcher.replaceYouMatcher iid whoMatcher)
      _ -> noMatch
    Matcher.PutLocationIntoPlay timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.PutLocationIntoPlay who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.LocationEntersPlay timing locationMatcher ->
      guardTiming timing $ \case
        Window.LocationEntersPlay locationId -> locationMatches iid source window' locationId locationMatcher
        _ -> noMatch
    Matcher.PlayerHasPlayableCard costStatus cardMatcher -> do
      -- This is the for the Painted
      -- TODO: do we need to grab the card source?
      -- cards <- filter (/= c) <$> getList cardMatcher
      cards <- select cardMatcher
      anyM (getIsPlayable iid source costStatus [window']) cards
    Matcher.PhaseBegins timing phaseMatcher -> guardTiming timing $ \case
      Window.AnyPhaseBegins -> pure $ phaseMatcher == Matcher.AnyPhase
      Window.PhaseBegins p -> matchPhase p phaseMatcher
      _ -> noMatch
    Matcher.PhaseEnds timing phaseMatcher -> guardTiming timing $ \case
      Window.PhaseEnds p -> matchPhase p phaseMatcher
      _ -> noMatch
    Matcher.PhaseStep timing phaseStepMatcher -> guardTiming timing $ \case
      Window.EnemiesAttackStep -> pure $ phaseStepMatcher == Matcher.EnemiesAttackStep
      Window.HuntersMoveStep -> pure $ phaseStepMatcher == Matcher.HuntersMoveStep
      _ -> noMatch
    Matcher.TurnBegins timing whoMatcher -> guardTiming timing $ \case
      Window.TurnBegins who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.TurnEnds timing whoMatcher -> guardTiming timing $ \case
      Window.TurnEnds who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.TurnWouldEnd timing whoMatcher -> guardTiming timing $ \case
      Window.WouldEndTurn who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.RoundBegins timing -> guardTiming timing (pure . (== Window.AtBeginningOfRound))
    Matcher.RoundEnds timing -> guardTiming timing (pure . (== Window.AtEndOfRound))
    Matcher.Enters timing whoMatcher whereMatcher -> guardTiming timing $ \case
      Window.Entering iid' lid ->
        andM
          [ matchWho iid iid' whoMatcher
          , locationMatches iid source window' lid whereMatcher
          ]
      _ -> noMatch
    Matcher.Leaves timing whoMatcher whereMatcher -> guardTiming timing $ \case
      Window.Leaving iid' lid ->
        andM
          [ matchWho iid iid' whoMatcher
          , locationMatches iid source window' lid whereMatcher
          ]
      _ -> noMatch
    Matcher.Moves timing whoMatcher sourceMatcher fromMatcher toMatcher ->
      guardTiming timing $ \case
        Window.Moves iid' source' mFromLid toLid -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , case (fromMatcher, mFromLid) of
                (Matcher.Anywhere, _) -> isMatch
                (_, Just fromLid) ->
                  locationMatches iid source window' fromLid fromMatcher
                _ -> noMatch
            , locationMatches iid source window' toLid toMatcher
            ]
        _ -> noMatch
    Matcher.WouldMove timing whoMatcher sourceMatcher fromMatcher toMatcher ->
      guardTiming timing $ \case
        Window.WouldMove iid' source' fromLid toLid -> do
          andM
            [ matchWho iid iid' whoMatcher
            , sourceMatches source' sourceMatcher
            , case fromMatcher of
                Matcher.Anywhere -> isMatch
                _ -> locationMatches iid source window' fromLid fromMatcher
            , locationMatches iid source window' toLid toMatcher
            ]
        _ -> noMatch
    Matcher.MoveAction timing whoMatcher fromMatcher toMatcher ->
      guardTiming timing $ \case
        Window.MoveAction iid' fromLid toLid ->
          andM
            [ matchWho iid iid' whoMatcher
            , locationMatches iid source window' fromLid fromMatcher
            , locationMatches iid source window' toLid toMatcher
            ]
        _ -> noMatch
    Matcher.PerformAction timing whoMatcher actionMatcher -> guardTiming timing $ \case
      Window.PerformAction iid' action ->
        andM [matchWho iid iid' whoMatcher, actionMatches iid action actionMatcher]
      _ -> noMatch
    Matcher.PerformedSameTypeOfAction timing whoMatcher actionMatcher -> guardTiming timing $ \case
      Window.PerformedSameTypeOfAction iid' actions ->
        andM [matchWho iid iid' whoMatcher, anyM (\a -> actionMatches iid a actionMatcher) actions]
      _ -> noMatch
    Matcher.WouldHaveSkillTestResult timing whoMatcher _ skillTestResultMatcher -> do
      -- The #when is questionable, but "Would" based timing really is
      -- only meant to have a When window
      let
        isWindowMatch = \case
          Matcher.ResultOneOf xs -> anyM isWindowMatch xs
          Matcher.FailureResult _ -> guardTiming timing $ \case
            Window.WouldFailSkillTest who -> matchWho iid who whoMatcher
            _ -> noMatch
          Matcher.SuccessResult _ -> guardTiming timing $ \case
            Window.WouldPassSkillTest who -> matchWho iid who whoMatcher
            _ -> noMatch
          Matcher.AnyResult -> guardTiming #when $ \case
            Window.WouldFailSkillTest who -> matchWho iid who whoMatcher
            Window.WouldPassSkillTest who -> matchWho iid who whoMatcher
            _ -> noMatch
      isWindowMatch skillTestResultMatcher
    Matcher.InitiatedSkillTest timing whoMatcher skillTypeMatcher skillValueMatcher skillTestMatcher ->
      guardTiming timing $ \case
        Window.InitiatedSkillTest st -> case skillTestType st of
          SkillSkillTest skillType | skillTypeMatches skillType skillTypeMatcher -> do
            andM
              [ matchWho iid (skillTestInvestigator st) whoMatcher
              , skillTestValueMatches
                  iid
                  (skillTestAction st)
                  (skillTestType st)
                  skillValueMatcher
              , skillTestMatches iid source st skillTestMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.SkillTestEnded timing whoMatcher skillTestMatcher -> guardTiming timing $ \case
      Window.SkillTestEnded skillTest ->
        andM
          [ matchWho iid (skillTestInvestigator skillTest) whoMatcher
          , skillTestMatches iid source skillTest skillTestMatcher
          ]
      _ -> noMatch
    Matcher.SkillTestResult timing whoMatcher skillMatcher skillTestResultMatcher ->
      do
        mskillTest <- getSkillTest
        matchSkillTest <- case mskillTest of
          Nothing -> noMatch
          Just st -> skillTestMatches iid source st skillMatcher
        if not matchSkillTest
          then noMatch
          else do
            let
              isWindowMatch = \case
                Matcher.ResultOneOf xs -> anyM isWindowMatch xs
                Matcher.FailureResult gameValueMatcher -> guardTiming timing $ \case
                  Window.FailInvestigationSkillTest who lid n -> case skillMatcher of
                    Matcher.WhileInvestigating whereMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , locationMatches iid source window' lid whereMatcher
                        ]
                    _ -> noMatch
                  Window.FailAttackEnemy who enemyId n -> case skillMatcher of
                    Matcher.WhileAttackingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , enemyMatches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.FailEvadeEnemy who enemyId n -> case skillMatcher of
                    Matcher.WhileEvadingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , enemyMatches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.FailSkillTest who n -> do
                    let unhandled = case skillMatcher of
                          Matcher.WhileAttackingAnEnemy _ -> False
                          Matcher.WhileEvadingAnEnemy _ -> False
                          Matcher.WhileInvestigating _ -> False
                          _ -> True
                    if unhandled
                      then
                        andM
                          [ matchWho iid who whoMatcher
                          , gameValueMatches n gameValueMatcher
                          ]
                      else noMatch
                  _ -> noMatch
                Matcher.SuccessResult gameValueMatcher -> guardTiming timing $ \case
                  Window.PassInvestigationSkillTest who lid n -> case skillMatcher of
                    Matcher.WhileInvestigating whereMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , locationMatches iid source window' lid whereMatcher
                        ]
                    _ -> noMatch
                  Window.SuccessfulAttackEnemy who _ enemyId n -> case skillMatcher of
                    Matcher.WhileAttackingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , enemyMatches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.SuccessfulEvadeEnemy who enemyId n -> case skillMatcher of
                    Matcher.WhileEvadingAnEnemy enemyMatcher ->
                      andM
                        [ matchWho iid who whoMatcher
                        , gameValueMatches n gameValueMatcher
                        , enemyMatches enemyId enemyMatcher
                        ]
                    _ -> noMatch
                  Window.PassSkillTest _ _ who n -> do
                    let unhandled = case skillMatcher of
                          Matcher.WhileAttackingAnEnemy _ -> False
                          Matcher.WhileEvadingAnEnemy _ -> False
                          Matcher.WhileInvestigating _ -> False
                          _ -> True
                    if unhandled
                      then
                        andM
                          [ matchWho iid who whoMatcher
                          , gameValueMatches n gameValueMatcher
                          ]
                      else noMatch
                  _ -> noMatch
                Matcher.AnyResult -> guardTiming timing $ \case
                  Window.FailSkillTest who _ -> matchWho iid who whoMatcher
                  Window.PassSkillTest _ _ who _ -> matchWho iid who whoMatcher
                  _ -> noMatch
            isWindowMatch skillTestResultMatcher
    Matcher.DuringTurn whoMatcher -> guardTiming #when $ \case
      Window.NonFast -> matchWho iid iid whoMatcher
      Window.DuringTurn who -> matchWho iid who whoMatcher
      Window.FastPlayerWindow -> do
        miid <- selectOne Matcher.TurnInvestigator
        case miid of
          Nothing -> pure False
          Just who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.OrWindowMatcher matchers ->
      anyM (windowMatches iid rawSource window') matchers
    Matcher.TreacheryEntersPlay timing treacheryMatcher -> guardTiming timing $ \case
      Window.TreacheryEntersPlay treacheryId -> treacheryId <=~> treacheryMatcher
      _ -> noMatch
    Matcher.EnemySpawns timing whereMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemySpawns enemyId locationId ->
          andM
            [ enemyMatches enemyId enemyMatcher
            , locationMatches iid source window' locationId whereMatcher
            ]
        _ -> noMatch
    Matcher.EnemyWouldAttack timing whoMatcher enemyAttackMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyWouldAttack details -> case attackTarget details of
          InvestigatorTarget who ->
            andM
              [ matchWho iid who whoMatcher
              , enemyMatches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.EnemyAttacks timing whoMatcher enemyAttackMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttacks details -> case attackTarget details of
          InvestigatorTarget who ->
            andM
              [ matchWho iid who whoMatcher
              , enemyMatches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.EnemyAttacksEvenIfCancelled timing whoMatcher enemyAttackMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttacksEvenIfCancelled details -> case attackTarget details of
          InvestigatorTarget who ->
            andM
              [ matchWho iid who whoMatcher
              , enemyMatches (attackEnemy details) enemyMatcher
              , enemyAttackMatches iid details enemyAttackMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.EnemyAttacked timing whoMatcher sourceMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyAttacked who attackSource enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , enemyMatches enemyId enemyMatcher
            , sourceMatches attackSource sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyAttackedSuccessfully timing whoMatcher sourceMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.SuccessfulAttackEnemy who source' enemyId _ -> do
          andM
            [ matchWho iid who whoMatcher
            , enemyMatches enemyId enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.AttemptToEvade timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.AttemptToEvadeEnemy who enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , enemyMatches enemyId enemyMatcher
            ]
        _ -> noMatch
    Matcher.EnemyEvaded timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEvaded who enemyId -> do
          -- we need to check defeated because things like Kymani's ability can discard them
          andM
            [ matchWho iid who whoMatcher
            , orM
                [ pure $ enemyMatcher == Matcher.AnyEnemy
                , enemyMatches enemyId enemyMatcher
                , enemyMatches enemyId (Matcher.OutOfPlayEnemy RemovedZone enemyMatcher)
                ]
            ]
        _ -> noMatch
    Matcher.EnemyEngaged timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEngaged who enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , enemyMatches enemyId enemyMatcher
            ]
        _ -> noMatch
    Matcher.MythosStep mythosStepMatcher -> guardTiming #when $ \case
      Window.AllDrawEncounterCard ->
        pure $ mythosStepMatcher == Matcher.WhenAllDrawEncounterCard
      Window.AfterCheckDoomThreshold ->
        pure $ mythosStepMatcher == Matcher.AfterCheckDoomThreshold
      _ -> noMatch
    Matcher.WouldRevealChaosToken timing whoMatcher -> guardTiming timing $ \case
      Window.WouldRevealChaosToken _ who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.RevealChaosToken timing whoMatcher tokenMatcher -> guardTiming timing $ \case
      Window.RevealChaosToken who token ->
        andM
          [ matchWho iid who whoMatcher
          , matchChaosToken who token tokenMatcher
          ]
      _ -> noMatch
    Matcher.ResolvesTreachery timing whoMatcher treacheryMatcher -> guardTiming timing $ \case
      Window.ResolvesTreachery who treacheryId ->
        andM [matchWho iid who whoMatcher, treacheryId <=~> treacheryMatcher]
      _ -> noMatch
    Matcher.ResolvesChaosToken timing whoMatcher tokenMatcher -> guardTiming timing $ \case
      Window.ResolvesChaosToken who token ->
        andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
      _ -> noMatch
    Matcher.CancelChaosToken timing whoMatcher tokenMatcher ->
      guardTiming timing $ \case
        Window.CancelChaosToken who token ->
          andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
        _ -> noMatch
    Matcher.IgnoreChaosToken timing whoMatcher tokenMatcher ->
      guardTiming timing $ \case
        Window.IgnoreChaosToken who token ->
          andM [matchWho iid who whoMatcher, matchChaosToken who token tokenMatcher]
        _ -> noMatch
    Matcher.AddedToVictory timing cardMatcher -> guardTiming timing $ \case
      Window.AddedToVictory card -> pure $ cardMatch card cardMatcher
      _ -> noMatch
    Matcher.AssetDefeated timing defeatedByMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.AssetDefeated assetId defeatedBy ->
          andM
            [ elem assetId <$> select assetMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDefeated timing whoMatcher defeatedByMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyDefeated (Just who) defeatedBy enemyId ->
          andM
            [ matchWho iid who whoMatcher
            , enemyMatches enemyId enemyMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        Window.EnemyDefeated Nothing defeatedBy enemyId | whoMatcher == Matcher.You -> do
          andM
            [ enemyMatches enemyId enemyMatcher
            , defeatedByMatches
                defeatedBy
                (defeatedByMatcher <> Matcher.BySource (Matcher.SourceOwnedBy $ Matcher.InvestigatorWithId iid))
            ]
        Window.EnemyDefeated Nothing defeatedBy enemyId | whoMatcher == Matcher.Anyone -> do
          andM
            [ enemyMatches
                enemyId
                enemyMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.EnemyEnters timing whereMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEnters enemyId lid ->
          andM
            [ enemyMatches enemyId enemyMatcher
            , locationMatches iid source window' lid whereMatcher
            ]
        _ -> noMatch
    Matcher.EnemyLeaves timing whereMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyLeaves enemyId lid ->
          andM
            [ enemyMatches enemyId enemyMatcher
            , locationMatches iid source window' lid whereMatcher
            ]
        _ -> noMatch
    Matcher.ChosenRandomLocation timing whereMatcher -> guardTiming timing $ \case
      Window.ChosenRandomLocation lid -> locationMatches iid source window' lid whereMatcher
      _ -> noMatch
    Matcher.EnemyWouldBeDefeated timing enemyMatcher -> guardTiming timing $ \case
      Window.EnemyWouldBeDefeated enemyId -> enemyMatches enemyId enemyMatcher
      _ -> noMatch
    Matcher.EnemyWouldReady timing enemyMatcher -> guardTiming timing $ \case
      Window.WouldReady (EnemyTarget enemyId) -> enemyMatches enemyId enemyMatcher
      _ -> noMatch
    Matcher.EnemyReadies timing enemyMatcher -> guardTiming timing $ \case
      Window.Readies (EnemyTarget enemyId) -> enemyMatches enemyId enemyMatcher
      _ -> noMatch
    Matcher.FastPlayerWindow -> guardTiming #when (pure . (== Window.FastPlayerWindow))
    Matcher.DealtDamageOrHorror timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.WouldTakeDamageOrHorror source' (InvestigatorTarget iid') _ _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.WouldTakeDamageOrHorror source' (AssetTarget aid) _ _ ->
        andM
          [ elem aid
              <$> select
                ( Matcher.AssetControlledBy
                    $ Matcher.replaceYouMatcher iid whoMatcher
                )
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.DealtDamage timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.DealtDamage source' _ (InvestigatorTarget iid') _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.DealtDamage source' _ (AssetTarget aid) _ ->
        andM
          [ elem aid
              <$> select
                (Matcher.AssetControlledBy $ Matcher.replaceYouMatcher iid whoMatcher)
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.DealtHorror timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.DealtHorror source' (InvestigatorTarget iid') _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.DealtHorror source' (AssetTarget aid) _ ->
        andM
          [ elem aid
              <$> select
                (Matcher.AssetControlledBy $ Matcher.replaceYouMatcher iid whoMatcher)
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.AssignedHorror timing whoMatcher targetListMatcher ->
      guardTiming timing $ \case
        Window.AssignedHorror _ who targets ->
          andM
            [ matchWho iid who whoMatcher
            , targetListMatches targets targetListMatcher
            ]
        _ -> noMatch
    Matcher.AssetDealtDamage timing sourceMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' _ (AssetTarget aid) _ ->
          andM
            [ elem aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.AssetDealtDamageOrHorror timing sourceMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' _ (AssetTarget aid) _ ->
          andM
            [ elem aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        Window.DealtHorror source' (AssetTarget aid) _ ->
          andM
            [ elem aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDealtDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' damageEffect (EnemyTarget eid) _ ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , elem eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDealtExcessDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.DealtExcessDamage source' damageEffect (EnemyTarget eid) _ ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , elem eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyTakeDamage timing damageEffectMatcher enemyMatcher valueMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeDamage source' damageEffect (EnemyTarget eid) n ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , elem eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.SpentClues timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.SpentClues who n ->
        andM
          [ matchWho iid who whoMatcher
          , gameValueMatches n valueMatcher
          ]
      _ -> noMatch
    Matcher.DiscoverClues timing whoMatcher whereMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.DiscoverClues who lid _ n ->
          andM
            [ matchWho iid who (Matcher.replaceThatLocation lid whoMatcher)
            , locationMatches iid source window' lid whereMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.GainsClues timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.GainsClues who _ n ->
        andM [matchWho iid who whoMatcher, gameValueMatches n valueMatcher]
      _ -> noMatch
    Matcher.GainsResources timing whoMatcher sourceMatcher valueMatcher -> guardTiming timing $ \case
      Window.GainsResources who source' n ->
        andM
          [ matchWho iid who whoMatcher
          , gameValueMatches n valueMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> noMatch
    Matcher.DiscoveringLastClue timing whoMatcher whereMatcher ->
      guardTiming timing $ \case
        Window.DiscoveringLastClue who lid ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' lid whereMatcher
            ]
        _ -> noMatch
    Matcher.LastClueRemovedFromAsset timing assetMatcher -> guardTiming timing $ \case
      Window.LastClueRemovedFromAsset aid -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.DrawsCards timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.DrawCards who cards ->
        andM
          [ matchWho iid who whoMatcher
          , gameValueMatches (length cards) valueMatcher
          ]
      _ -> noMatch
    Matcher.DrawCard timing whoMatcher cardMatcher deckMatcher ->
      guardTiming timing $ \case
        Window.DrawCard who card deck ->
          andM
            [ matchWho iid who whoMatcher
            , case cardMatcher of
                Matcher.BasicCardMatch baseMatcher ->
                  pure $ cardMatch card baseMatcher
                _ -> elem card <$> select cardMatcher
            , deckMatch iid deck deckMatcher
            ]
        _ -> noMatch
    Matcher.WouldDrawCard timing whoMatcher deckMatcher ->
      guardTiming timing $ \case
        Window.WouldDrawCard who deck ->
          andM
            [ matchWho iid who whoMatcher
            , deckMatch iid deck $ Matcher.replaceThatInvestigator who deckMatcher
            ]
        _ -> noMatch
    Matcher.DeckWouldRunOutOfCards timing whoMatcher -> guardTiming timing $ \case
      Window.DeckWouldRunOutOfCards who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.DeckHasNoCards timing whoMatcher -> guardTiming timing $ \case
      Window.DeckHasNoCards who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.EncounterDeckRunsOutOfCards -> pure $ wType == Window.EncounterDeckRunsOutOfCards
    Matcher.PlayCard timing whoMatcher cardMatcher -> guardTiming timing $ \case
      Window.PlayCard who cardPlay ->
        andM
          [ matchWho iid who whoMatcher
          , case cardMatcher of
              Matcher.BasicCardMatch baseMatcher ->
                pure $ cardMatch cardPlay.card baseMatcher
              _ ->
                elem cardPlay.card <$> select (Matcher.basic (Matcher.CardWithId cardPlay.card.id) <> cardMatcher)
          ]
      _ -> noMatch
    Matcher.PlayEventDiscarding timing whoMatcher eventMatcher -> guardTiming timing $ \case
      Window.PlayEventDiscarding who event ->
        andM
          [ matchWho iid who whoMatcher
          , event <=~> eventMatcher
          ]
      _ -> noMatch
    Matcher.AgendaEntersPlay timing agendaMatcher -> guardTiming timing $ \case
      Window.EnterPlay (AgendaTarget aid) -> elem aid <$> select agendaMatcher
      _ -> noMatch
    Matcher.AssetEntersPlay timing assetMatcher -> guardTiming timing $ \case
      Window.EnterPlay (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.AssetLeavesPlay timing assetMatcher -> guardTiming timing $ \case
      Window.LeavePlay (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.EnemyEntersPlay timing enemyMatcher -> guardTiming timing $ \case
      Window.EnterPlay (EnemyTarget eid) -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.LocationLeavesPlay timing locationMatcher -> guardTiming timing $ \case
      Window.LeavePlay (LocationTarget aid) -> elem aid <$> select locationMatcher
      _ -> noMatch
    Matcher.EnemyLeavesPlay timing enemyMatcher -> guardTiming timing $ \case
      Window.LeavePlay (EnemyTarget eid) -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.Explored timing whoMatcher resultMatcher -> guardTiming timing $ \case
      Window.Explored who result ->
        andM
          [ matchWho iid who whoMatcher
          , case resultMatcher of
              Matcher.SuccessfulExplore locationMatcher -> case result of
                Window.Success lid -> lid <=~> locationMatcher
                Window.Failure _ -> noMatch
              Matcher.FailedExplore cardMatcher -> case result of
                Window.Success _ -> noMatch
                Window.Failure card -> pure $ cardMatch card cardMatcher
          ]
      _ -> noMatch
    Matcher.AttemptExplore timing whoMatcher -> guardTiming timing $ \case
      Window.AttemptExplore who -> matchWho iid who whoMatcher
      _ -> noMatch

matchWho
  :: HasGame m
  => InvestigatorId
  -> InvestigatorId
  -> Matcher.InvestigatorMatcher
  -> m Bool
matchWho iid who Matcher.You = pure $ iid == who
matchWho iid who Matcher.NotYou = pure $ iid /= who
matchWho _ _ Matcher.Anyone = pure True
matchWho iid who (Matcher.InvestigatorAt matcher) = do
  who <=~> Matcher.InvestigatorAt (Matcher.replaceYouMatcher iid matcher)
matchWho iid who matcher = do
  matcher' <- replaceMatchWhoLocations iid (Matcher.replaceYouMatcher iid matcher)
  who <=~> matcher'
 where
  replaceMatchWhoLocations iid' = \case
    Matcher.InvestigatorAt matcher' -> do
      pure $ Matcher.InvestigatorAt $ Matcher.replaceYouMatcher iid matcher'
    Matcher.HealableInvestigator source damageType inner -> do
      Matcher.HealableInvestigator source damageType
        <$> replaceMatchWhoLocations iid' inner
    other -> pure other

gameValueMatches :: HasGame m => Int -> Matcher.ValueMatcher -> m Bool
gameValueMatches n = \case
  Matcher.AnyValue -> pure True
  Matcher.LessThan gv -> (n <) <$> getPlayerCountValue gv
  Matcher.GreaterThan gv -> (n >) <$> getPlayerCountValue gv
  Matcher.LessThanOrEqualTo gv -> (n <=) <$> getPlayerCountValue gv
  Matcher.GreaterThanOrEqualTo gv -> (n >=) <$> getPlayerCountValue gv
  Matcher.EqualTo gv -> (n ==) <$> getPlayerCountValue gv

skillTestValueMatches
  :: HasGame m
  => InvestigatorId
  -> Maybe Action
  -> SkillTestType
  -> Matcher.SkillTestValueMatcher
  -> m Bool
skillTestValueMatches iid maction skillTestType = \case
  Matcher.AnySkillTestValue -> pure True
  Matcher.SkillTestGameValue valueMatcher -> do
    maybe (pure False) (`gameValueMatches` valueMatcher) =<< getSkillTestDifficulty
  Matcher.GreaterThanBaseValue -> do
    getSkillTestDifficulty >>= \case
      Nothing -> pure False
      Just n -> case skillTestType of
        SkillSkillTest skillType -> do
          baseSkill <- baseSkillValueFor skillType maction iid
          pure $ n > baseSkill
        AndSkillTest types -> do
          baseSkill <- sum <$> traverse (\skillType -> baseSkillValueFor skillType maction iid) types
          pure $ n > baseSkill
        ResourceSkillTest -> do
          resources <- field InvestigatorResources iid
          pure $ n > resources
        BaseValueSkillTest x _ -> pure $ n > x

enemyMatches :: HasGame m => EnemyId -> Matcher.EnemyMatcher -> m Bool
enemyMatches !enemyId !mtchr = elem enemyId <$> select mtchr

chaosTokenMatches :: HasGame m => ChaosToken -> Matcher.ChaosTokenMatcher -> m Bool
chaosTokenMatches !tkn !mtchr = elem tkn <$> select mtchr

locationMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Window
  -> LocationId
  -> Matcher.LocationMatcher
  -> m Bool
locationMatches investigatorId source window locationId matcher' = do
  let matcher = Matcher.replaceYouMatcher investigatorId matcher'
  case matcher of
    -- special cases
    Matcher.NotLocation m -> not <$> locationMatches investigatorId source window locationId m
    Matcher.LocationWithEnemy enemyMatcher -> selectAny $ Matcher.enemyAt locationId <> enemyMatcher
    Matcher.LocationWithAsset assetMatcher -> selectAny $ Matcher.assetAt locationId <> assetMatcher
    Matcher.LocationWithInvestigator whoMatcher -> selectAny $ Matcher.investigatorAt locationId <> whoMatcher
    Matcher.LocationWithoutTreachery treacheryMatcher -> do selectNone $ Matcher.treacheryAt locationId <> treacheryMatcher
    Matcher.LocationWithTreachery treacheryMatcher -> do selectAny $ Matcher.treacheryAt locationId <> treacheryMatcher

    -- normal cases
    Matcher.ConnectedToSetAsideLocation {} -> locationId <=~> matcher
    Matcher.UnbarricadedConnectedFrom {} -> locationId <=~> matcher
    Matcher.LocationWithCardsUnderneath {} -> locationId <=~> matcher
    Matcher.FloodedLocation {} -> locationId <=~> matcher
    Matcher.FullyFloodedLocation {} -> locationId <=~> matcher
    Matcher.PartiallyFloodedLocation {} -> locationId <=~> matcher
    Matcher.CanHaveFloodLevelIncreased {} -> locationId <=~> matcher
    Matcher.LocationFartherFrom {} -> locationId <=~> matcher
    Matcher.LocationFartherFromMatching {} -> locationId <=~> matcher
    Matcher.LocationBetween {} -> locationId <=~> matcher
    Matcher.LocationWithDistanceFromAtLeast {} -> locationId <=~> matcher
    Matcher.LocationWithAccessiblePath {} -> locationId <=~> matcher
    Matcher.CanMoveCloserToLocation {} -> locationId <=~> matcher
    Matcher.LocationWithDistanceFromAtMost {} -> locationId <=~> matcher
    Matcher.LocationWhenCriteria {} -> locationId <=~> matcher
    Matcher.LocationBeingDiscovered {} -> locationId <=~> matcher
    Matcher.CanMoveToLocation {} -> locationId <=~> matcher
    Matcher.CanEnterLocation _ -> locationId <=~> matcher
    Matcher.IncludeEmptySpace _ -> locationId <=~> matcher
    Matcher.LocationCanBeEnteredBy {} -> locationId <=~> matcher
    Matcher.LocationWithAdjacentBarrier -> locationId <=~> matcher
    Matcher.MostBreaches _ -> locationId <=~> matcher
    Matcher.LocationWithVictory -> locationId <=~> matcher
    Matcher.FewestBreaches {} -> locationId <=~> matcher
    Matcher.RearmostLocation {} -> locationId <=~> matcher
    Matcher.LocationInRow {} -> locationId <=~> matcher
    Matcher.LocationInPosition {} -> locationId <=~> matcher
    Matcher.LocationWithBreaches _ -> locationId <=~> matcher
    Matcher.LocationWithBrazier _ -> locationId <=~> matcher
    Matcher.LocationWithIncursion -> locationId <=~> matcher
    Matcher.LocationWithDefeatedEnemyThisRound -> locationId <=~> matcher
    Matcher.LocationWithDiscoverableCluesBy _ -> locationId <=~> matcher
    Matcher.LocationWithoutModifier _ -> locationId <=~> matcher
    Matcher.LocationWithModifier _ -> locationId <=~> matcher
    Matcher.IsIchtacasDestination -> locationId <=~> matcher
    Matcher.HauntedLocation -> locationId <=~> matcher
    Matcher.LocationWithToken _ -> locationId <=~> matcher
    Matcher.SingleSidedLocation -> locationId <=~> matcher
    Matcher.LocationWithLowerPrintedShroudThan _ -> locationId <=~> matcher
    Matcher.LocationNotInPlay -> locationId <=~> matcher
    Matcher.LocationWithLabel _ -> locationId <=~> matcher
    Matcher.LocationWithTitle _ -> locationId <=~> matcher
    Matcher.LocationWithFullTitle _ _ -> locationId <=~> matcher
    Matcher.LocationWithSymbol _ -> locationId <=~> matcher
    Matcher.LocationWithUnrevealedTitle _ -> locationId <=~> matcher
    Matcher.LocationWithId _ -> locationId <=~> matcher
    Matcher.LocationIs _ -> locationId <=~> matcher
    Matcher.Anywhere -> locationId <=~> matcher
    Matcher.Nowhere -> locationId <=~> matcher
    Matcher.LocationCanBeFlipped -> locationId <=~> matcher
    Matcher.BlockedLocation -> locationId <=~> matcher
    Matcher.EmptyLocation -> locationId <=~> matcher
    Matcher.LocationWithCardId _ -> locationId <=~> matcher
    Matcher.LocationWithoutInvestigators -> locationId <=~> matcher
    Matcher.LocationWithoutEnemies -> locationId <=~> matcher
    Matcher.AccessibleLocation -> locationId <=~> matcher
    Matcher.AccessibleFrom _ -> locationId <=~> matcher
    Matcher.AccessibleTo _ -> locationId <=~> matcher
    Matcher.ConnectedFrom _ -> locationId <=~> matcher
    Matcher.ConnectedTo _ -> locationId <=~> matcher
    Matcher.ConnectedLocation -> locationId <=~> matcher
    Matcher.RevealedLocation -> locationId <=~> matcher
    Matcher.UnrevealedLocation -> locationId <=~> matcher
    Matcher.FarthestLocationFromInvestigator _ _ -> locationId <=~> matcher
    Matcher.FarthestLocationFromLocation _ _ -> locationId <=~> matcher
    Matcher.NearestLocationToLocation _ _ -> locationId <=~> matcher
    Matcher.FarthestLocationFromAll _ -> locationId <=~> matcher
    Matcher.NearestLocationToYou _ -> locationId <=~> matcher
    Matcher.NearestLocationTo _ _ -> locationId <=~> matcher
    Matcher.LocationWithTrait _ -> locationId <=~> matcher
    Matcher.LocationWithoutTrait _ -> locationId <=~> matcher
    Matcher.LocationInDirection _ _ -> locationId <=~> matcher
    Matcher.ClosestPathLocation _ _ -> locationId <=~> matcher
    Matcher.ClosestUnbarricadedPathLocation _ _ -> locationId <=~> matcher
    Matcher.LocationWithoutClues -> locationId <=~> matcher
    Matcher.HighestShroud _ -> locationId <=~> matcher
    Matcher.LocationWithDamage {} -> locationId <=~> matcher
    Matcher.LocationWithDistanceFrom {} -> locationId <=~> matcher
    Matcher.LocationWithAnyKeys -> locationId <=~> matcher
    Matcher.LocationWithKey _ -> locationId <=~> matcher
    Matcher.LocationWithMostInvestigators _ -> locationId <=~> matcher
    Matcher.LocationWithClues valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues locationId
    Matcher.LocationWithDoom valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationDoom locationId
    Matcher.LocationWithHorror valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationHorror locationId
    Matcher.LocationWithShroud valueMatcher ->
      field LocationShroud locationId >>= \case
        Nothing -> pure False
        Just shroud -> gameValueMatches shroud valueMatcher
    Matcher.LocationWithAttachedEvent {} -> locationId <=~> matcher
    Matcher.LocationWithShroudLessThanOrEqualToLessThanEnemyMaybeField {} -> locationId <=~> matcher
    Matcher.LocationWithMostClues locationMatcher ->
      elem locationId
        <$> select (Matcher.LocationWithMostClues locationMatcher)
    Matcher.LocationWithResources valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationResources locationId
    Matcher.LocationLeavingPlay -> case windowType window of
      Window.LeavePlay (LocationTarget lid) ->
        pure $ locationId == lid
      _ -> error "invalid window for LocationLeavingPlay"
    Matcher.SameLocation -> do
      let
        getSameLocationSource = \case
          EnemySource eid -> field EnemyLocation eid
          AssetSource aid -> field AssetLocation aid
          AbilitySource s _ -> getSameLocationSource s
          _ -> error $ "can't detect same location for source " <> show source

      mlid' <- getSameLocationSource source
      pure $ Just locationId == mlid'
    Matcher.YourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId == yourLocationId
    Matcher.ThisLocation ->
      let
        go = \case
          LocationSource lid -> pure $ lid == locationId
          IndexedSource _ s -> go s
          ProxySource s _ -> go s
          AbilitySource s _ -> go s
          _ -> error $ "Invalid source for ThisLocation: " <> show source
       in
        go source
    Matcher.NotYourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId /= yourLocationId
    Matcher.LocationMatchAll ms ->
      allM (locationMatches investigatorId source window locationId) ms
    Matcher.LocationMatchAny ms ->
      anyM (locationMatches investigatorId source window locationId) ms
    Matcher.FirstLocation ms ->
      anyM (locationMatches investigatorId source window locationId) ms -- a bit weird here since first means nothing
    Matcher.InvestigatableLocation -> do
      modifiers <- getModifiers (LocationTarget locationId)
      pure $ CannotInvestigate `notElem` modifiers
    Matcher.LocationIsInFrontOf _ -> locationId <=~> matcher
    Matcher.ThatLocation -> error "That Location needs to be replaced"

skillTestMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestMatches iid source st mtchr = case Matcher.replaceYouMatcher iid mtchr of
  Matcher.PerilousSkillTest -> getIsPerilous st
  Matcher.IfSkillTestMatcher p thenMatcher elseMatcher -> do
    p' <- skillTestMatches iid source st p
    skillTestMatches iid source st $ if p' then thenMatcher else elseMatcher
  Matcher.SkillTestWithDifficulty gv ->
    getSkillTestDifficulty >>= \case
      Nothing -> pure False
      Just n -> gameValueMatches n gv
  Matcher.SkillTestOnEncounterCard -> skillTestSource st `sourceMatches` Matcher.EncounterCardSource
  Matcher.NotSkillTest matcher ->
    not <$> skillTestMatches iid source st matcher
  Matcher.AnySkillTest -> pure True
  Matcher.SkillTestWasFailed -> pure $ case skillTestResult st of
    FailedBy _ _ -> True
    _ -> False
  Matcher.YourSkillTest matcher ->
    liftA2
      (&&)
      (pure $ skillTestInvestigator st == iid)
      (skillTestMatches iid source st matcher)
  Matcher.UsingThis -> pure $ case skillTestSource st of
    AbilitySource (ProxySource _ s) _ -> s == source
    AbilitySource s _ -> s == source
    ProxySource (CardIdSource _) s -> s == source
    ProxySource _ s -> s == source
    IndexedSource _ s -> s == source
    s -> s == source
  Matcher.SkillTestSourceMatches sourceMatcher ->
    sourceMatches (skillTestSource st) sourceMatcher
  Matcher.SkillTestBeforeRevealingChaosTokens ->
    pure $ null $ skillTestRevealedChaosTokens st
  Matcher.SkillTestWithRevealedChaosToken matcher ->
    anyM (`chaosTokenMatches` Matcher.IncludeSealed matcher)
      $ skillTestRevealedChaosTokens st
  Matcher.SkillTestWithRevealedChaosTokenCount n matcher ->
    (>= n)
      <$> countM
        (`chaosTokenMatches` Matcher.IncludeSealed matcher)
        (skillTestRevealedChaosTokens st)
  Matcher.SkillTestOnCardWithTrait t -> elem t <$> sourceTraits (skillTestSource st)
  Matcher.SkillTestOnCard match -> (`cardMatch` match) <$> sourceToCard (skillTestSource st)
  Matcher.SkillTestOnLocation match -> case skillTestSource st of
    AbilitySource s n | n < 100 -> case s.location of
      Just lid -> lid <=~> match
      Nothing -> pure False
    _ -> pure False
  Matcher.SkillTestWithResolvedChaosTokenBy whoMatcher matcher -> do
    iids <- select whoMatcher
    anyM (`chaosTokenMatches` Matcher.IncludeSealed matcher)
      . filter (maybe False (`elem` iids) . chaosTokenRevealedBy)
      $ skillTestRevealedChaosTokens st
  Matcher.SkillTestFromRevelation -> pure $ skillTestIsRevelation st
  Matcher.SkillTestWithAction actionMatcher -> case skillTestAction st of
    Just action -> actionMatches iid action actionMatcher
    Nothing -> pure False
  Matcher.WhileInvestigating locationMatcher -> case skillTestAction st of
    Just Action.Investigate -> case skillTestTarget st of
      LocationTarget lid -> elem lid <$> select locationMatcher
      ProxyTarget (LocationTarget lid) _ ->
        elem lid <$> select locationMatcher
      BothTarget (LocationTarget lid1) (LocationTarget lid2) -> do
        selectAny $ locationMatcher <> Matcher.mapOneOf Matcher.LocationWithId [lid1, lid2]
      _ -> pure False
    _ -> pure False
  Matcher.SkillTestOnTreachery treacheryMatcher -> case st.source.treachery of
    Just tid -> elem tid <$> select treacheryMatcher
    _ -> pure False
  Matcher.SkillTestOnAsset assetMatcher -> case st.source.asset of
    Just aid -> elem aid <$> select assetMatcher
    _ -> pure False
  Matcher.WhileAttackingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Fight -> case st.target.enemy of
      Just eid -> elem eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.WhileEvadingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Evade -> case st.target.enemy of
      Just eid -> elem eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.WhileParleyingWithAnEnemy enemyMatcher ->
    case st.target.enemy of
      Just eid -> andM [isParley, elem eid <$> select enemyMatcher]
      _ -> pure False
  Matcher.WhileParleying -> isParley
  Matcher.SkillTestWithSkill sk -> selectAny sk
  Matcher.SkillTestWithSkillType sType -> pure $ case skillTestType st of
    SkillSkillTest sType' -> sType' == sType
    AndSkillTest types -> sType `elem` types
    ResourceSkillTest -> False
    BaseValueSkillTest _ _ -> False
  Matcher.SkillTestAtYourLocation -> do
    canAffectOthers <- withoutModifier iid CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
    mlid1 <- field InvestigatorLocation iid
    mlid2 <- field InvestigatorLocation st.investigator
    case (mlid1, mlid2) of
      (Just lid1, Just lid2) ->
        pure $ lid1 == lid2 && (canAffectOthers || iid == st.investigator)
      _ -> pure False
  Matcher.SkillTestAt locationMatcher -> targetMatches st.target (Matcher.TargetAtLocation locationMatcher)
  Matcher.SkillTestOfInvestigator whoMatcher -> st.investigator <=~> whoMatcher
  Matcher.SkillTestMatches ms -> allM (skillTestMatches iid source st) ms
  Matcher.SkillTestOneOf ms -> anyM (skillTestMatches iid source st) ms

matchPhase :: Monad m => Phase -> Matcher.PhaseMatcher -> m Bool
matchPhase p = \case
  Matcher.AnyPhase -> pure True
  Matcher.IsMythosPhase -> case p of
    MythosPhase {} -> pure True
    _ -> pure False
  Matcher.IsEnemyPhase -> case p of
    EnemyPhase {} -> pure True
    _ -> pure False
  Matcher.IsInvestigationPhase -> case p of
    InvestigationPhase {} -> pure True
    _ -> pure False
  Matcher.IsUpkeepPhase -> case p of
    UpkeepPhase {} -> pure True
    _ -> pure False

getModifiedChaosTokenFaces :: HasGame m => [ChaosToken] -> m [ChaosTokenFace]
getModifiedChaosTokenFaces tokens = concatMapM getModifiedChaosTokenFace tokens

getModifiedChaosTokenFace :: HasGame m => ChaosToken -> m [ChaosTokenFace]
getModifiedChaosTokenFace token = do
  modifiers' <- getModifiers (ChaosTokenTarget token)
  pure $ foldl' applyModifier [chaosTokenFace token] modifiers'
 where
  applyModifier _ (ChaosTokenFaceModifier fs') = fs'
  applyModifier [f'] (ForcedChaosTokenChange f fs) | f == f' = fs
  applyModifier fs _ = fs

cardListMatches :: HasGame m => [Card] -> Matcher.CardListMatcher -> m Bool
cardListMatches cards = \case
  Matcher.AnyCards -> pure $ notNull cards
  Matcher.LengthIs valueMatcher -> gameValueMatches (length cards) valueMatcher
  Matcher.DifferentLengthIsAtLeast n cardMatcher -> pure $ length (nubOrdOn toTitle $ filter (`cardMatch` cardMatcher) cards) >= n
  Matcher.HasCard cardMatcher -> pure $ any (`cardMatch` cardMatcher) cards
  Matcher.NoCards -> pure $ null cards

targetListMatches
  :: HasGame m => [Target] -> Matcher.TargetListMatcher -> m Bool
targetListMatches targets = \case
  Matcher.AnyTargetList -> pure True
  Matcher.HasTarget targetMatcher ->
    anyM (`targetMatches` targetMatcher) targets
  Matcher.ExcludesTarget targetMatcher ->
    noneM (`targetMatches` targetMatcher) targets

deckMatch
  :: HasGame m
  => InvestigatorId
  -> DeckSignifier
  -> Matcher.DeckMatcher
  -> m Bool
deckMatch iid deckSignifier = \case
  Matcher.EncounterDeck -> pure $ deckSignifier == EncounterDeck
  Matcher.DeckOf investigatorMatcher -> case deckSignifier of
    Deck.InvestigatorDeck iid' -> matchWho iid iid' investigatorMatcher
    _ -> pure False
  Matcher.AnyDeck -> pure True
  Matcher.DeckIs deckSignifier' -> pure $ deckSignifier == deckSignifier'
  Matcher.DeckOneOf matchers' -> anyM (deckMatch iid deckSignifier) matchers'

agendaMatches :: HasGame m => AgendaId -> Matcher.AgendaMatcher -> m Bool
agendaMatches _ Matcher.AnyAgenda = pure True
agendaMatches !agendaId mtchr = elem agendaId <$> select mtchr

actMatches :: HasGame m => ActId -> Matcher.ActMatcher -> m Bool
actMatches _ Matcher.AnyAct = pure True
actMatches !actId mtchr = elem actId <$> select mtchr

actionMatches :: HasGame m => InvestigatorId -> Action -> Matcher.ActionMatcher -> m Bool
actionMatches _ _ Matcher.AnyAction = pure True
actionMatches _ a (Matcher.ActionIs a') = pure $ a == a'
actionMatches iid a (Matcher.ActionMatches as) = allM (actionMatches iid a) as
actionMatches iid a (Matcher.ActionOneOf as) = anyM (actionMatches iid a) as
actionMatches iid a Matcher.RepeatableAction = do
  a' <- getAttrs @Investigator iid
  actions <- withModifiers iid (toModifiers GameSource [ActionCostModifier (-1)]) $ do
    getActions iid (defaultWindows iid)

  playableCards <-
    filter (`cardMatch` Matcher.NotCard Matcher.FastCard)
      <$> getPlayableCards a' (UnpaidCost NoAction) (defaultWindows iid)

  canAffordTakeResources <- withModifiers iid (toModifiers GameSource [ActionCostOf IsAnyAction (-1)]) $ do
    getCanAfford a' [#resource]

  canAffordDrawCards <- withModifiers iid (toModifiers GameSource [ActionCostOf IsAnyAction (-1)]) $ do
    getCanAfford a' [#draw]
  let available = filter (elem a . abilityActions) actions
  canDraw <- canDo iid #draw
  canTakeResource <- canDo iid #resource
  canPlay <- canDo iid #play
  pure
    $ or
      [ notNull available
      , canAffordTakeResources && canTakeResource && a == #resource
      , canAffordDrawCards && canDraw && a == #draw
      , canPlay && notNull playableCards && a == #play
      ]

skillTypeMatches :: SkillType -> Matcher.SkillTypeMatcher -> Bool
skillTypeMatches st = \case
  Matcher.AnySkillType -> True
  Matcher.NotSkillType st' -> st /= st'
  Matcher.IsSkillType st' -> st == st'
  Matcher.SkillTypeOneOf ss -> st `elem` ss

enemyAttackMatches
  :: HasGame m => InvestigatorId -> EnemyAttackDetails -> Matcher.EnemyAttackMatcher -> m Bool
enemyAttackMatches youId details@EnemyAttackDetails {..} = \case
  Matcher.EnemyAttackMatches as -> allM (enemyAttackMatches youId details) as
  Matcher.AnyEnemyAttack -> pure True
  Matcher.NotEnemyAttack inner -> not <$> enemyAttackMatches youId details inner
  Matcher.AttackOfOpportunityAttack -> pure $ attackType == AttackOfOpportunity
  Matcher.AttackDealtDamageOrHorror -> pure $ any ((> 0) . uncurry max) (toList attackDamaged)
  Matcher.AttackDamagedAsset inner -> do
    flip anyM (mapToList attackDamaged) \(target, (x, y)) -> case target of
      AssetTarget aid | x > 0 || y > 0 -> aid <=~> inner
      _ -> pure False
  Matcher.AttackOfOpportunityAttackYouProvoked ->
    pure $ attackType == AttackOfOpportunity && isTarget youId attackOriginalTarget
  Matcher.AttackViaAlert -> pure $ attackType == AlertAttack
  Matcher.AttackViaSource sourceMatcher -> sourceMatches details.source sourceMatcher
  Matcher.CancelableEnemyAttack matcher -> do
    modifiers' <- getModifiers (sourceToTarget attackSource)
    enemyModifiers <- getModifiers attackEnemy
    andM
      [ pure $ EffectsCannotBeCanceled `notElem` modifiers'
      , pure $ AttacksCannotBeCancelled `notElem` enemyModifiers
      , enemyAttackMatches youId details matcher
      ]

damageEffectMatches
  :: Monad m => DamageEffect -> Matcher.DamageEffectMatcher -> m Bool
damageEffectMatches a = \case
  Matcher.AnyDamageEffect -> pure True
  Matcher.AttackDamageEffect -> pure $ a == AttackDamageEffect
  Matcher.NonAttackDamageEffect -> pure $ a == NonAttackDamageEffect

spawnAtOneOf
  :: (HasGame m, HasQueue Message m) => Maybe InvestigatorId -> EnemyId -> [LocationId] -> m ()
spawnAtOneOf miid eid targetLids = do
  locations' <- select $ Matcher.IncludeEmptySpace Matcher.Anywhere
  player <- maybe getLeadPlayer getPlayer miid
  case targetLids `List.intersect` locations' of
    [] -> push (toDiscard GameSource eid)
    [lid] -> do
      windows' <- checkWindows [mkWhen (Window.EnemyWouldSpawnAt eid lid)]
      pushAll $ windows' : resolve (EnemySpawn miid lid eid)
    lids -> do
      windowPairs <- for lids $ \lid -> do
        windows' <- checkWindows [mkWhen (Window.EnemyWouldSpawnAt eid lid)]
        pure (windows', lid)

      push
        $ chooseOne
          player
          [ targetLabel lid $ windows' : resolve (EnemySpawn miid lid eid)
          | (windows', lid) <- windowPairs
          ]

sourceCanDamageEnemy :: HasGame m => EnemyId -> Source -> m Bool
sourceCanDamageEnemy eid source = do
  modifiers' <- getModifiers (EnemyTarget eid)
  not <$> anyM prevents modifiers'
 where
  prevents = \case
    CannotBeDamagedByPlayerSourcesExcept matcher ->
      not
        <$> sourceMatches
          source
          (Matcher.SourceMatchesAny [Matcher.EncounterCardSource, matcher])
    CannotBeDamagedByPlayerSources matcher ->
      sourceMatches
        source
        (Matcher.SourceMatchesAny [Matcher.EncounterCardSource, matcher])
    CannotBeDamaged -> pure True
    _ -> pure False

sourceCanDamageAsset :: HasGame m => AssetId -> Source -> m Bool
sourceCanDamageAsset eid source = do
  mods <- getModifiers eid
  not <$> anyM prevents mods
 where
  prevents = \case
    CannotBeDamagedBySourcesExcept matcher -> not <$> sourceMatches source matcher
    _ -> pure False

getPotentialSlots
  :: (HasGame m, IsCard a) => a -> InvestigatorId -> m [SlotType]
getPotentialSlots card iid = do
  slots <- field InvestigatorSlots iid
  let
    slotTypesAndSlots :: [(SlotType, Slot)] =
      concatMap (\(slotType, slots') -> map (slotType,) slots')
        $ mapToList slots
    passesRestriction = \case
      RestrictedSlot _ matcher _ -> cardMatch card matcher
      Slot {} -> True
      AdjustableSlot {} -> True
  map fst
    <$> filterM
      ( \(_, slot) ->
          if passesRestriction slot
            then case slotItems slot of
              [] -> pure True
              (x : xs) -> do
                mods <- getModifiers x
                let canFit = \case
                      SharesSlotWith n matcher -> length xs + 1 < n && cardMatch card matcher
                      _ -> False
                let willFit = any canFit mods
                orM
                  [ allM (<=~> Matcher.DiscardableAsset) (x : xs) -- either all can be discarded
                  , pure willFit
                  ]
            else pure False
      )
      slotTypesAndSlots

defeatedByMatches
  :: HasGame m => DefeatedBy -> Matcher.DefeatedByMatcher -> m Bool
defeatedByMatches defeatedBy = \case
  Matcher.ByAnyOf xs -> anyM (defeatedByMatches defeatedBy) xs
  Matcher.ByHorror -> pure $ wasDefeatedByHorror defeatedBy
  Matcher.ByDamage -> pure $ wasDefeatedByDamage defeatedBy
  Matcher.ByOther -> pure $ wasDefeatedByOther defeatedBy
  Matcher.BySource sourceMatcher -> sourceMatches (defeatedBySource defeatedBy) sourceMatcher
  Matcher.ByAny -> pure True
  Matcher.DefeatedByMatches xs -> allM (defeatedByMatches defeatedBy) xs
  Matcher.NotBy x -> not <$> defeatedByMatches defeatedBy x

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

sourceMatches
  :: (HasCallStack, HasGame m) => Source -> Matcher.SourceMatcher -> m Bool
sourceMatches s = \case
  Matcher.SourceIsCancelable sm -> case s of
    CardCostSource _ -> pure False
    other -> do
      modifiers' <- maybe (pure []) getModifiers (sourceToMaybeTarget other)
      andM [pure $ EffectsCannotBeCanceled `notElem` modifiers', sourceMatches s sm]
  Matcher.SourceIs s' -> pure $ s == s'
  Matcher.NotSource matcher -> not <$> sourceMatches s matcher
  Matcher.SourceMatchesAny ms -> anyM (sourceMatches s) ms
  Matcher.SourceWithTrait t -> elem t <$> sourceTraits s
  Matcher.SourceIsEnemyAttack em -> case s of
    EnemyAttackSource eid -> elem eid <$> select em
    _ -> pure False
  Matcher.SourceIsTreacheryEffect tm -> case s of
    TreacherySource tid -> elem tid <$> select tm
    _ -> pure False
  Matcher.SourceIsEnemy em -> case s.enemy of
    Nothing -> pure False
    Just eid -> eid <=~> em
  Matcher.SourceIsLocation lm -> case s.location of
    Nothing -> pure False
    Just lid -> lid <=~> lm
  Matcher.SourceIsAsset am ->
    let
      isAssetSource s' = case s' of
        AssetSource aid -> elem aid <$> select am
        AbilitySource (AssetSource aid) _ -> elem aid <$> select am
        ProxySource (CardIdSource _) pSource -> isAssetSource pSource
        IndexedSource _ pSource -> isAssetSource pSource
        ProxySource pSource _ -> isAssetSource pSource
        BothSource lSource rSource -> orM [isAssetSource lSource, isAssetSource rSource]
        _ -> pure False
     in
      isAssetSource s
  Matcher.SourceIsEvent am ->
    let
      isEventSource s' = case s' of
        EventSource aid -> elem aid <$> select am
        AbilitySource (EventSource aid) _ -> elem aid <$> select am
        ProxySource (CardIdSource _) pSource -> isEventSource pSource
        ProxySource pSource _ -> isEventSource pSource
        IndexedSource _ pSource -> isEventSource pSource
        BothSource lSource rSource -> orM [isEventSource lSource, isEventSource rSource]
        _ -> pure False
     in
      isEventSource s
  Matcher.AnySource -> pure True
  Matcher.SourceMatches ms -> allM (sourceMatches s) ms
  Matcher.SourceOwnedBy whoMatcher ->
    let
      checkSource = \case
        AbilitySource source' _ -> checkSource source'
        AssetSource aid ->
          selectAssetController aid >>= \case
            Just iid' -> elem iid' <$> select whoMatcher
            _ -> pure False
        EventSource eid -> do
          selectEventController eid >>= \case
            Just controllerId -> elem controllerId <$> select whoMatcher
            Nothing -> do
              -- event may have been discarded already
              mOwner <- join . fmap toCardOwner <$> fieldMay EventCard eid
              case mOwner of
                Just owner -> elem owner <$> select whoMatcher
                Nothing -> pure False
        SkillSource sid -> do
          selectSkillController sid >>= \case
            Just controllerId -> elem controllerId <$> select whoMatcher
            Nothing -> pure False
        InvestigatorSource iid -> elem iid <$> select whoMatcher
        ElderSignEffectSource iid -> elem iid <$> select whoMatcher
        CardIdSource cid -> do
          c <- getCard cid
          case toCardOwner c of
            Nothing -> pure False
            Just iid -> elem iid <$> select whoMatcher
        _ -> pure False
     in
      checkSource s
  Matcher.SourceIsCardEffect -> do
    let
      go = \case
        ChaosTokenSource {} -> True
        ChaosTokenEffectSource {} -> True
        ElderSignEffectSource {} -> True
        ActiveCostSource {} -> False
        AbilitySource {} -> True
        ActSource {} -> True
        ActDeckSource {} -> False
        AgendaDeckSource {} -> False
        AgendaSource {} -> True
        AgendaMatcherSource {} -> True
        AssetMatcherSource {} -> True
        ActMatcherSource {} -> True
        AssetSource {} -> True
        CardCodeSource {} -> False
        CardIdSource {} -> False
        DeckSource {} -> False
        EffectSource {} -> True
        EmptyDeckSource {} -> False
        EncounterCardSource {} -> False
        EnemyAttackSource {} -> True
        EnemySource {} -> True
        EventSource {} -> True
        GameSource -> False
        InvestigatorSource {} -> True
        LocationMatcherSource {} -> True
        EnemyMatcherSource {} -> True
        LocationSource {} -> True
        IndexedSource _ s' -> go s'
        ProxySource (CardIdSource _) s' -> go s'
        ProxySource s' _ -> go s'
        ResourceSource {} -> False
        ScenarioSource {} -> False
        SkillSource {} -> True
        SkillTestSource {} -> False
        StorySource {} -> True
        TestSource {} -> True
        TreacherySource {} -> True
        YouSource {} -> False
        CampaignSource {} -> False
        ThisCard {} -> True
        CardCostSource {} -> False
        BothSource a b -> go a || go b
        TarotSource {} -> True
        BatchSource {} -> False
    pure $ go s
  Matcher.SourceIsType t -> case t of
    AssetType -> case s of
      AssetSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == AssetType
      _ -> pure False
    EventType -> case s of
      EventSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EventType
      _ -> pure False
    SkillType -> case s of
      SkillSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == SkillType
      _ -> pure False
    PlayerTreacheryType -> case s of
      TreacherySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == PlayerTreacheryType
      _ -> pure False
    PlayerEnemyType -> case s of
      EnemySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == PlayerEnemyType
      _ -> pure False
    TreacheryType -> case s of
      TreacherySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == TreacheryType
      _ -> pure False
    EnemyType -> case s of
      EnemySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EnemyType
      _ -> pure False
    LocationType -> case s of
      LocationSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == LocationType
      _ -> pure False
    EncounterAssetType -> case s of
      AssetSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EncounterAssetType
      _ -> pure False
    EncounterEventType -> case s of
      EventSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EncounterEventType
      _ -> pure False
    ActType -> case s of
      ActSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == ActType
      _ -> pure False
    AgendaType -> case s of
      AgendaSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == AgendaType
      _ -> pure False
    StoryType -> case s of
      StorySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == StoryType
      _ -> pure False
    InvestigatorType -> case s of
      InvestigatorSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == InvestigatorType
      _ -> pure False
    ScenarioType -> case s of
      ScenarioSource -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == ScenarioType
      _ -> pure False
  Matcher.EncounterCardSource ->
    let
      check = \case
        AbilitySource source' _ -> check source'
        ActSource _ -> True
        AgendaSource _ -> True
        EnemySource _ -> True
        LocationSource _ -> True
        TreacherySource _ -> True
        _ -> False
     in
      pure $ check s
  Matcher.SourceIsPlayerCard ->
    let
      check = \case
        AbilitySource source' _ -> check source'
        AssetSource _ -> True
        EventSource _ -> True
        SkillSource _ -> True
        InvestigatorSource _ -> True
        _ -> False
     in
      pure $ check s
  Matcher.SourceIsPlayerCardAbility ->
    case s of
      AbilitySource s' _ -> sourceMatches s' Matcher.SourceIsPlayerCard
      _ -> pure False
  Matcher.SourceWithCard cardMatcher -> do
    let
      getCardSource = \case
        AbilitySource source' _ -> getCardSource source'
        AssetSource aid -> fieldMay AssetCard aid
        EventSource eid -> fieldMay EventCard eid
        SkillSource sid -> fieldMay SkillCard sid
        EnemySource eid -> fieldMay EnemyCard eid
        TreacherySource tid -> fieldMay TreacheryCard tid
        LocationSource lid -> fieldMay LocationCard lid
        StorySource sid -> fieldMay StoryCard sid
        InvestigatorSource _ -> pure Nothing
        CardIdSource cid -> Just <$> getCard cid
        _ -> pure Nothing
    mCard <- getCardSource s
    pure $ case mCard of
      Just c -> c `cardMatch` cardMatcher
      Nothing -> False

historyMatches :: HasGame m => Matcher.HistoryMatcher -> History -> m Bool
historyMatches = \case
  Matcher.DefeatedEnemiesWithTotalHealth vMatcher ->
    (`gameValueMatches` vMatcher) . sum . map defeatedEnemyHealth . historyEnemiesDefeated

canDo :: HasGame m => InvestigatorId -> Action -> m Bool
canDo iid action = do
  mods <- getModifiers iid
  let
    prevents = \case
      CannotTakeAction x -> preventsAction x
      MustTakeAction x -> not <$> preventsAction x -- reads a little weird but we want only thing things x would prevent with cannot take action
      CannotDrawCards -> pure $ action == #draw
      CannotDrawCardsFromPlayerCardEffects -> pure $ action == #draw
      CannotManipulateDeck -> pure $ action == #draw
      CannotGainResources -> pure $ action == #resource
      _ -> pure False
    preventsAction = \case
      FirstOneOfPerformed as | action `elem` as -> do
        fieldP InvestigatorActionsPerformed (\taken -> all (\a -> all (notElem a) taken) as) iid
      FirstOneOfPerformed {} -> pure False
      IsAction action' -> pure $ action == action'
      AnyActionTarget as -> anyM preventsAction as
      EnemyAction {} -> pure False
      IsAnyAction {} -> pure True

  not <$> anyM prevents mods

getCanMoveTo :: (Sourceable source, HasGame m) => InvestigatorId -> source -> LocationId -> m Bool
getCanMoveTo iid source lid = elem lid <$> getCanMoveToLocations iid source

getCanMoveToLocations
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m [LocationId]
getCanMoveToLocations iid source = do
  canMove <-
    iid <=~> (Matcher.InvestigatorCanMove <> not_ (Matcher.InVehicleMatching Matcher.AnyAsset))
  if canMove
    then do
      selectOne (Matcher.locationWithInvestigator iid) >>= \case
        Nothing -> pure []
        Just lid -> do
          imods <- getModifiers iid
          mods <- getModifiers lid
          ls <-
            select
              $ Matcher.canEnterLocation iid
              <> Matcher.NotLocation (Matcher.LocationWithId lid)
          let extraCostsToLeave = mconcat [c | AdditionalCostToLeave c <- mods]
          flip filterM ls $ \l -> do
            mods' <- getModifiers l
            pcosts <- filterM ((l <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
            revealed' <- field LocationRevealed l
            baseEnter <- mwhen (not revealed') <$> field LocationCostToEnterUnrevealed l -- Added for cards like Nimble
            let extraCostsToEnter = baseEnter <> concatMap snd pcosts <> mconcat [c | AdditionalCostToEnter c <- mods']
            getCanAffordCost iid source [#move] [] (extraCostsToLeave <> extraCostsToEnter)
    else pure []

getCanMoveToMatchingLocations
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Matcher.LocationMatcher
  -> m [LocationId]
getCanMoveToMatchingLocations iid source matcher = do
  ls <- getCanMoveToLocations iid source
  filter (`elem` ls) <$> select matcher

getConnectedMoveLocations
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m [LocationId]
getConnectedMoveLocations iid source =
  getCanMoveToMatchingLocations
    iid
    source
    (Matcher.ConnectedFrom $ Matcher.locationWithInvestigator iid)

getAccessibleLocations
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m [LocationId]
getAccessibleLocations iid source =
  getCanMoveToMatchingLocations
    iid
    source
    (Matcher.AccessibleFrom $ Matcher.locationWithInvestigator iid)

getCanLeaveCurrentLocation :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m Bool
getCanLeaveCurrentLocation iid source = do
  mLocation <- selectOne $ Matcher.locationWithInvestigator iid
  case mLocation of
    Nothing -> pure False
    Just lid -> do
      mods <- getModifiers lid
      let extraCostsToLeave = mconcat [c | AdditionalCostToLeave c <- mods]
      getCanAffordCost iid source [#move] [] extraCostsToLeave

getOtherPlayersPlayableCards :: HasGame m => InvestigatorId -> CostStatus -> [Window] -> m [Card]
getOtherPlayersPlayableCards iid costStatus windows' = do
  mods <- getModifiers iid
  forMaybeM mods $ \case
    PlayableCardOf _ c -> do
      playable <- getIsPlayable iid (toSource iid) costStatus windows' c
      pure $ guard playable $> c
    _ -> pure Nothing

damageTypeMatches :: DamageStrategy -> Matcher.DamageTypeMatcher -> Bool
damageTypeMatches strategy = \case
  Matcher.IsDirectDamage -> isDirectStrategy
  Matcher.IsNonDirectDamage -> not isDirectStrategy
  Matcher.AnyDamageType -> True
 where
  isDirectStrategy = case strategy of
    DamageDirect -> True
    DamageAny -> False
    DamageAssetsFirst -> False
    DamageFirst _ -> False
    SingleTarget -> False
    DamageEvenly -> False
    DamageFromHastur -> False

passesLimits :: HasGame m => InvestigatorId -> Card -> m Bool
passesLimits iid c = allM go (cdLimits $ toCardDef c)
 where
  go = \case
    LimitPerInvestigator m -> case toCardType c of
      AssetType -> do
        n <- selectCount $ Matcher.assetControlledBy iid <> Matcher.AssetWithTitle (nameTitle $ toName c)
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    LimitPerTrait t m -> case toCardType c of
      AssetType -> do
        n <- selectCount (Matcher.assetControlledBy iid <> Matcher.AssetWithTrait t)
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    MaxPerAttack m -> case toCardType c of
      EventType -> do
        n <- selectCount $ Matcher.eventIs c
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    MaxPerGame m -> do
      n <- getCardUses (toCardCode c)
      pure $ m > n
    MaxPerTurn m -> do
      n <- getCardUses (toCardCode c)
      pure $ m > n
    MaxPerRound m -> do
      n <- getCardUses (toCardCode c)
      pure $ m > n
    MaxPerTraitPerRound t m -> do
      n <- count (elem t) . map toTraits <$> getAllCardUses
      pure $ m > n
