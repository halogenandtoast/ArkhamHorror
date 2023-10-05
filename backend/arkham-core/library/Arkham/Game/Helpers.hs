module Arkham.Game.Helpers (
  module Arkham.Game.Helpers,
  module X,
) where

import Arkham.Prelude

import Arkham.Helpers.Ability as X
import Arkham.Helpers.EncounterSet as X
import Arkham.Helpers.Log as X
import Arkham.Helpers.Modifiers as X
import Arkham.Helpers.Query as X
import Arkham.Helpers.Scenario as X
import Arkham.Helpers.Slot as X
import Arkham.Helpers.Source as X
import Arkham.Helpers.Window as X
import Arkham.Helpers.Xp as X

import Arkham.Ability
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types (Field (..))
import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses (useTypeCount)
import Arkham.Attack
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes hiding (isMatch)
import Arkham.Cost.FieldCost
import Arkham.Criteria qualified as Criteria
import Arkham.DamageEffect
import Arkham.Deck hiding (InvestigatorDeck, InvestigatorDiscard)
import Arkham.DefeatedBy
import Arkham.Effect.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.Game
import Arkham.Game.Settings
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Investigator (additionalActionCovers, baseSkillValueFor)
import Arkham.Helpers.Message hiding (AssetDamage, InvestigatorDamage, PaidCost)
import Arkham.Helpers.Tarot
import Arkham.Id
import Arkham.Investigator.Types (Field (..), InvestigatorAttrs (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types hiding (location)
import Arkham.Matcher qualified as Matcher
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
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
import Arkham.Token (countTokens)
import Arkham.Token qualified as Token
import Arkham.Trait (Trait, toTraits)
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window
import Control.Lens (over)
import Control.Monad.Reader (local)
import Data.Data.Lens (biplate)
import Data.List.Extra (nubOrdOn)
import Data.Set qualified as Set

replaceThisCard :: Card -> Source -> Source
replaceThisCard c = \case
  ThisCard -> CardCostSource (toCardId c)
  s -> s

cancelChaosToken :: HasQueue Message m => ChaosToken -> m ()
cancelChaosToken token = withQueue_ $ \queue ->
  filter
    ( \case
        When (RevealChaosToken _ _ token') | token == token' -> False
        RevealChaosToken _ _ token' | token == token' -> False
        After (RevealChaosToken _ _ token') | token == token' -> False
        RequestedChaosTokens _ _ [token'] | token == token' -> False
        RequestedChaosTokens {} -> error "not setup for multiple tokens"
        _ -> True
    )
    queue

getPlayableCards
  :: (HasCallStack, HasGame m)
  => InvestigatorAttrs
  -> CostStatus
  -> [Window]
  -> m [Card]
getPlayableCards a costStatus windows' = do
  asIfInHandCards <- getAsIfInHandCards (toId a)
  playableDiscards <- getPlayableDiscards a costStatus windows'
  hand <- field InvestigatorHand (toId a)
  playableHandCards <-
    filterM
      (getIsPlayable (toId a) (toSource a) costStatus windows')
      (hand <> asIfInHandCards)
  pure $ playableHandCards <> playableDiscards

getPlayableDiscards
  :: HasGame m => InvestigatorAttrs -> CostStatus -> [Window] -> m [Card]
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
        (zip @_ @Int [0 ..] investigatorDiscard)
  canPlayFromDiscard modifiers (n, card) =
    cdPlayableFromDiscard (toCardDef card)
      || any (allowsPlayFromDiscard n card) modifiers
  allowsPlayFromDiscard 0 card (CanPlayTopmostOfDiscard (mcardType, traits)) =
    let cardMatcher = maybe Matcher.AnyCard Matcher.CardWithType mcardType <> foldMap Matcher.CardWithTrait traits
        allMatches = filter (`cardMatch` cardMatcher) investigatorDiscard
     in case allMatches of
          (topmost : _) -> topmost == card
          _ -> False
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
  :: HasGame m => InvestigatorId -> Window -> Ability -> m Bool
getCanPerformAbility !iid !window !ability = do
  -- can perform an ability means you can afford it
  -- it is in the right window
  -- passes restrictions
  abilityModifiers <- getModifiers (AbilityTarget iid ability)
  let
    mAction = case abilityType ability of
      ActionAbilityWithBefore _ mBeforeAction _ -> mBeforeAction
      _ -> abilityAction ability
    additionalCosts = flip mapMaybe abilityModifiers $ \case
      AdditionalCost x -> Just x
      _ -> Nothing
    cost = abilityCost ability
    criteria = foldr setCriteria (abilityCriteria ability) abilityModifiers
    setCriteria :: ModifierType -> Criterion -> Criterion
    setCriteria = \case
      SetAbilityCriteria (CriteriaOverride c) -> const c
      _ -> id
  andM
    [ getCanAffordCost iid (toSource ability) mAction [window] cost
    , meetsActionRestrictions iid window ability
    , windowMatches iid (toSource ability) window (abilityWindow ability)
    , passesCriteria iid Nothing (abilitySource ability) [window] criteria
    , allM
        (getCanAffordCost iid (abilitySource ability) mAction [window])
        additionalCosts
    , not <$> preventedByInvestigatorModifiers iid ability
    ]

preventedByInvestigatorModifiers
  :: HasGame m => InvestigatorId -> Ability -> m Bool
preventedByInvestigatorModifiers iid ability = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  anyM prevents modifiers
 where
  prevents = \case
    CannotTakeAction x -> preventsAbility x
    MustTakeAction x -> not <$> preventsAbility x -- reads a little weird but we want only thing things x would prevent with cannot take action
    _ -> pure False
  preventsAbility = \case
    FirstOneOfPerformed as -> case abilityAction ability of
      Just action
        | action `elem` as ->
            fieldP InvestigatorActionsTaken (\taken -> all (`notElem` taken) as) iid
      _ -> pure False
    IsAction a -> pure $ abilityAction ability == Just a
    EnemyAction a matcher -> case abilitySource ability of
      EnemySource eid ->
        if abilityAction ability == Just a then eid <=~> matcher else pure False
      _ -> pure False

meetsActionRestrictions
  :: HasGame m => InvestigatorId -> Window -> Ability -> m Bool
meetsActionRestrictions iid _ ab@Ability {..} = go abilityType
 where
  go = \case
    Haunted -> pure False
    Cosmos -> pure False
    Objective aType -> go aType
    ForcedWhen _ aType -> go aType
    ActionAbilityWithBefore _ mBeforeAction cost ->
      go $ ActionAbility mBeforeAction cost
    ActionAbilityWithSkill mAction _ cost -> go $ ActionAbility mAction cost
    ActionAbility (Just action) _ -> do
      isTurn <- matchWho iid iid Matcher.TurnInvestigator
      if not isTurn
        then pure False
        else canDoAction iid ab action
    ActionAbility Nothing _ -> matchWho iid iid Matcher.TurnInvestigator
    FastAbility' _ (Just action) -> canDoAction iid ab action
    FastAbility' _ Nothing -> pure True
    ReactionAbility _ _ -> pure True
    ForcedAbility _ -> pure True
    SilentForcedAbility _ -> pure True
    ForcedAbilityWithCost _ _ -> pure True
    AbilityEffect _ -> pure True

canDoAction :: HasGame m => InvestigatorId -> Ability -> Action -> m Bool
canDoAction iid ab@Ability {abilitySource, abilityIndex} = \case
  Action.Fight -> case abilitySource of
    EnemySource _ -> pure True
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
        [o] -> notNull <$> select (Matcher.CanFightEnemyWithOverride o)
        _ -> error "multiple overrides found"
  Action.Engage -> case abilitySource of
    EnemySource _ -> pure True
    _ -> notNull <$> select Matcher.CanEngageEnemy
  Action.Parley -> case abilitySource of
    EnemySource _ -> pure True
    AssetSource _ -> pure True
    LocationSource _ -> pure True
    _ -> notNull <$> select (Matcher.CanParleyEnemy iid)
  Action.Investigate -> case abilitySource of
    LocationSource _ -> pure True
    _ -> notNull <$> select Matcher.InvestigatableLocation
  -- The below actions may not be handled correctly yet
  Action.Ability -> pure True
  Action.Draw -> pure True
  Action.Move -> pure True
  Action.Play -> pure True
  Action.Resign -> pure True
  Action.Resource -> pure True
  Action.Explore ->
    iid <=~> Matcher.InvestigatorWithoutModifier CannotExplore
  Action.Circle -> pure True

getCanAffordAbility
  :: (HasCallStack, HasGame m) => InvestigatorId -> Ability -> Window -> m Bool
getCanAffordAbility iid ability window =
  andM [getCanAffordUse iid ability window, getCanAffordAbilityCost iid ability]

getCanAffordAbilityCost :: HasGame m => InvestigatorId -> Ability -> m Bool
getCanAffordAbilityCost iid a@Ability {..} = do
  modifiers <- getModifiers (AbilityTarget iid a)
  let
    costF =
      case find isSetCost modifiers of
        Just (SetAbilityCost c) -> const c
        _ -> id
    isSetCost = \case
      SetAbilityCost _ -> True
      _ -> False
  go costF abilityType
 where
  go f = \case
    Haunted -> pure True
    Cosmos -> pure True
    ActionAbility mAction cost ->
      getCanAffordCost iid (toSource a) mAction [] (f cost)
    ActionAbilityWithSkill mAction _ cost ->
      getCanAffordCost iid (toSource a) mAction [] (f cost)
    ActionAbilityWithBefore _ mBeforeAction cost ->
      getCanAffordCost iid (toSource a) mBeforeAction [] (f cost)
    ReactionAbility _ cost -> getCanAffordCost iid (toSource a) Nothing [] (f cost)
    FastAbility' cost mAction -> getCanAffordCost iid (toSource a) mAction [] (f cost)
    ForcedAbilityWithCost _ cost ->
      getCanAffordCost iid (toSource a) Nothing [] (f cost)
    ForcedAbility _ -> pure True
    SilentForcedAbility _ -> pure True
    AbilityEffect _ -> pure True
    Objective {} -> pure True
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
getCanAffordUse :: (HasCallStack, HasGame m) => InvestigatorId -> Ability -> Window -> m Bool
getCanAffordUse = getCanAffordUseWith id CanIgnoreAbilityLimit

-- Use `f` to modify use count, used for `getWindowSkippable` to exclude the current call
-- EMAIL: Cards can't react to themselves, i.e. Grotesque Statue (4)
getCanAffordUseWith
  :: (HasCallStack, HasGame m)
  => ([UsedAbility] -> [UsedAbility])
  -> CanIgnoreAbilityLimit
  -> InvestigatorId
  -> Ability
  -> Window
  -> m Bool
getCanAffordUseWith f canIgnoreAbilityLimit iid ability window = do
  usedAbilities <-
    fmap f . filterDepthSpecificAbilities =<< field InvestigatorUsedAbilities iid
  limit <- getAbilityLimit iid ability
  ignoreLimit <-
    or
      . sequence [(IgnoreLimit `elem`), (CanIgnoreLimit `elem`)]
      <$> getModifiers (AbilityTarget iid ability)
  if ignoreLimit && canIgnoreAbilityLimit == CanIgnoreAbilityLimit
    then pure True
    else case limit of
      NoLimit -> do
        let
          go = \case
            ReactionAbility _ _ ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ForcedWhen _ aType -> go aType
            ForcedAbility _ -> pure $ notElem ability (map usedAbility usedAbilities)
            SilentForcedAbility _ ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ForcedAbilityWithCost _ _ ->
              pure $ notElem ability (map usedAbility usedAbilities)
            ActionAbility _ _ -> pure True
            ActionAbilityWithBefore {} -> pure True
            ActionAbilityWithSkill {} -> pure True
            FastAbility' {} -> pure True
            AbilityEffect _ -> pure True
            Objective {} -> pure True
            Haunted -> pure True
            Cosmos -> pure True
        go (abilityType ability)
      PlayerLimit (PerSearch trait) n -> do
        traitMatchingUsedAbilities <-
          filterM
            (fmap (elem trait) . sourceTraits . abilitySource . usedAbility)
            usedAbilities
        let usedCount = sum $ map usedTimes traitMatchingUsedAbilities
        pure $ usedCount < n
      PlayerLimit _ n ->
        pure
          . (< n)
          . maybe 0 usedTimes
          $ find
            ((== ability) . usedAbility)
            usedAbilities
      PerCopyLimit cardDef _ n -> do
        let
          abilityCardDef = \case
            PerCopyLimit cDef _ _ -> Just cDef
            _ -> Nothing
        pure
          . (< n)
          . getSum
          . foldMap (Sum . usedTimes)
          $ filter
            ((Just cardDef ==) . abilityCardDef . abilityLimit . usedAbility)
            usedAbilities
      PerInvestigatorLimit _ n -> do
        -- This is difficult and based on the window, so we need to match out the
        -- relevant investigator ids from the window. If this becomes more prevalent
        -- we may want a method from `Window -> Maybe InvestigatorId`
        case windowType window of
          Window.CommittedCard iid' _ -> do
            let
              matchingPerInvestigatorCount =
                flip count usedAbilities $ \usedAbility' ->
                  flip any (usedAbilityWindows usedAbility') $ \case
                    (windowType -> Window.CommittedCard iid'' _) -> usedAbility usedAbility' == ability && iid' == iid''
                    _ -> False
            pure $ matchingPerInvestigatorCount < n
          _ -> error "Unhandled per investigator limit"
      GroupLimit _ n -> do
        usedAbilities' <-
          fmap (map usedAbility)
            . filterDepthSpecificAbilities
            =<< concatMapM (field InvestigatorUsedAbilities)
            =<< allInvestigatorIds
        let total = count (== ability) usedAbilities'
        pure $ total < n

applyActionCostModifier
  :: [Action] -> [Action] -> Maybe Action -> ModifierType -> Int -> Int
applyActionCostModifier _ _ (Just action) (ActionCostOf (IsAction action') m) n
  | action == action' = n + m
applyActionCostModifier _ performedActions (Just action) (ActionCostOf (FirstOneOfPerformed as) m) n
  | action `elem` as && all (`notElem` performedActions) as =
      n + m
applyActionCostModifier _ _ _ (ActionCostModifier m) n = n + m
applyActionCostModifier _ _ _ _ n = n

getCanAffordCost
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Maybe Action
  -> [Window]
  -> Cost
  -> m Bool
getCanAffordCost iid (toSource -> source) mAction windows' = \case
  Free -> pure True
  UpTo {} -> pure True
  DiscardHandCost {} -> pure True
  DiscardTopOfDeckCost {} -> pure True
  AdditionalActionsCost {} -> pure True
  RevealCost {} -> pure True
  Costs xs ->
    and <$> traverse (getCanAffordCost iid source mAction windows') xs
  OrCost xs ->
    or <$> traverse (getCanAffordCost iid source mAction windows') xs
  ExhaustCost target -> case target of
    AssetTarget aid ->
      member aid <$> select Matcher.AssetReady
    EventTarget eid ->
      member eid <$> select Matcher.EventReady
    _ -> error $ "Not handled" <> show target
  ExhaustAssetCost matcher ->
    selectAny $ matcher <> Matcher.AssetReady
  DiscardAssetCost matcher ->
    selectAny $ matcher <> Matcher.DiscardableAsset
  UseCost assetMatcher uType n -> do
    assets <- selectList assetMatcher
    uses <-
      sum <$> traverse (fmap (useTypeCount uType) . field AssetUses) assets
    pure $ uses >= n
  DynamicUseCost assetMatcher uType useCost -> case useCost of
    DrawnCardsValue -> do
      let
        toDrawnCards = \case
          (windowType -> Window.DrawCards _ xs) -> length xs
          _ -> 0
        drawnCardsValue = sum $ map toDrawnCards windows'
      assets <- selectList assetMatcher
      uses <-
        sum <$> traverse (fmap (useTypeCount uType) . field AssetUses) assets
      pure $ uses >= drawnCardsValue
  UseCostUpTo assetMatcher uType n _ -> do
    assets <- selectList assetMatcher
    uses <-
      sum <$> traverse (fmap (useTypeCount uType) . field AssetUses) assets
    pure $ uses >= n
  ActionCost n -> do
    modifiers <- getModifiers (InvestigatorTarget iid)
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        takenActions <- field InvestigatorActionsTaken iid
        performedActions <- field InvestigatorActionsPerformed iid
        let
          modifiedActionCost =
            foldr (applyActionCostModifier takenActions performedActions mAction) n modifiers
        additionalActions <- field InvestigatorAdditionalActions iid
        additionalActionCount <-
          length
            <$> filterM
              (additionalActionCovers source mAction)
              additionalActions
        actionCount <- field InvestigatorRemainingActions iid
        pure $ (actionCount + additionalActionCount) >= modifiedActionCost
  ClueCost gameValue -> do
    spendableClues <- getSpendableClueCount [iid]
    totalClueCost <- getPlayerCountValue gameValue
    pure $ spendableClues >= totalClueCost
  ClueCostX -> do
    spendableClues <- getSpendableClueCount [iid]
    pure $ spendableClues >= 1
  PlaceClueOnLocationCost n -> do
    spendableClues <- getSpendableClueCount [iid]
    pure $ spendableClues >= n
  GroupClueCost n locationMatcher -> do
    cost <- getPlayerCountValue n
    iids <- selectList $ Matcher.InvestigatorAt locationMatcher
    totalSpendableClues <- getSpendableClueCount iids
    pure $ totalSpendableClues >= cost
  GroupClueCostRange (cost, _) locationMatcher -> do
    iids <- selectList $ Matcher.InvestigatorAt locationMatcher
    totalSpendableClues <- getSpendableClueCount iids
    pure $ totalSpendableClues >= cost
  IncreaseCostOfThis cardId n -> do
    card <- getCard cardId
    cost <- getModifiedCardCost iid card
    resources <- getSpendableResources iid
    pure $ resources >= (cost + n)
  ResourceCost n -> do
    resources <- getSpendableResources iid
    pure $ resources >= n
  ScenarioResourceCost n -> do
    resources <- scenarioFieldMap ScenarioTokens (countTokens Token.Resource)
    pure $ resources >= n
  DiscardFromCost n zone cardMatcher -> do
    -- We need to check that n valid candidates exist across all zones
    -- the logic is that we'll grab all card defs from each zone and then
    -- filter
    let
      getCards = \case
        FromHandOf whoMatcher ->
          fmap (filter (`cardMatch` cardMatcher) . concat)
            . traverse (field InvestigatorHand)
            =<< selectList whoMatcher
        FromPlayAreaOf whoMatcher -> do
          assets <- selectList $ Matcher.AssetControlledBy whoMatcher
          traverse (field AssetCard) assets
        CostZones zs -> concatMapM getCards zs
    (> n) . length <$> getCards zone
  DiscardCost _ _ -> pure True -- TODO: Make better
  DiscardCardCost _ -> pure True -- TODO: Make better
  DiscardDrawnCardCost -> pure True -- TODO: Make better
  ExileCost _ -> pure True -- TODO: Make better
  RemoveCost _ -> pure True -- TODO: Make better
  HorrorCost {} -> pure True -- TODO: Make better
  HorrorCostX {} -> pure True -- TODO: Make better
  DamageCost {} -> pure True -- TODO: Make better
  DirectDamageCost {} -> pure True -- TODO: Make better
  InvestigatorDamageCost {} -> pure True -- TODO: Make better
  DoomCost {} -> pure True -- TODO: Make better
  EnemyDoomCost _ enemyMatcher -> selectAny enemyMatcher
  SkillIconCost n skillTypes -> do
    handCards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    let
      total =
        sum
          $ map
            (count (`member` insertSet WildIcon skillTypes) . cdSkills . toCardDef)
            handCards
    pure $ total >= n
  DiscardCombinedCost n -> do
    handCards <-
      mapMaybe (preview _PlayerCard)
        . filter (`cardMatch` Matcher.NonWeakness)
        <$> field InvestigatorHand iid
    let
      total = sum $ map (maybe 0 toPrintedCost . cdCost . toCardDef) handCards
    pure $ total >= n
  ShuffleDiscardCost n cardMatcher -> do
    discards <-
      fieldMap
        InvestigatorDiscard
        (filter (`cardMatch` cardMatcher))
        iid
    pure $ length discards >= n
  HandDiscardCost n cardMatcher -> do
    cards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    pure $ length (filter (`cardMatch` cardMatcher) cards) >= n
  HandDiscardAnyNumberCost cardMatcher -> do
    cards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    pure $ length (filter (`cardMatch` cardMatcher) cards) > 0
  ReturnMatchingAssetToHandCost assetMatcher -> selectAny assetMatcher
  ReturnAssetToHandCost assetId -> selectAny $ Matcher.AssetWithId assetId
  SealCost tokenMatcher -> do
    tokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
    anyM (\token -> matchChaosToken iid token tokenMatcher) tokens
  SealChaosTokenCost _ -> pure True
  ReleaseChaosTokensCost n -> do
    case source of
      AssetSource aid -> fieldMap AssetSealedChaosTokens ((>= n) . length) aid
      _ -> error "Unhandled release token cost source"
  ReleaseChaosTokenCost t -> do
    case source of
      AssetSource aid -> fieldMap AssetSealedChaosTokens (elem t) aid
      _ -> error "Unhandled release token cost source"
  FieldResourceCost (FieldCost mtchr fld) -> do
    n <- getSum <$> selectAgg Sum fld mtchr
    resources <- getSpendableResources iid
    pure $ resources >= n
  SupplyCost locationMatcher supply ->
    iid
      <=~> ( Matcher.InvestigatorWithSupply supply
              <> Matcher.InvestigatorAt locationMatcher
           )
  ResolveEachHauntedAbility _ -> pure True

getActions :: (HasGame m, HasCallStack) => InvestigatorId -> Window -> m [Ability]
getActions iid window = getActionsWith iid window id

getActionsWith
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Window
  -> (Ability -> Ability)
  -> m [Ability]
getActionsWith iid window f = do
  modifiersForFilter <- getModifiers iid
  let
    abilityFilters =
      mapMaybe
        ( \case
            CannotTriggerAbilityMatching m -> Just m
            _ -> Nothing
        )
        modifiersForFilter
  unfilteredActions <- map f . nub <$> getAllAbilities
  actions' <-
    if null abilityFilters
      then pure unfilteredActions
      else do
        ignored <- select (mconcat abilityFilters)
        pure $ filter (`notMember` ignored) unfilteredActions
  actionsWithSources <-
    concat <$> for
      actions'
      \action -> do
        case abilitySource action of
          ProxySource (AgendaMatcherSource m) base -> do
            sources <- selectListMap AgendaSource m
            pure
              $ map
                (\source -> action {abilitySource = ProxySource source base})
                sources
          ProxySource (AssetMatcherSource m) base -> do
            sources <- selectListMap AssetSource m
            pure
              $ map
                (\source -> action {abilitySource = ProxySource source base})
                sources
          ProxySource (LocationMatcherSource m) base -> do
            sources <- selectListMap LocationSource m
            pure
              $ map
                (\source -> action {abilitySource = ProxySource source base})
                sources
          ProxySource (EnemyMatcherSource m) base -> do
            sources <- selectListMap EnemySource m
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
          blankPrevents _ = False
          -- If the window is fast we only permit fast abilities, but forced
          -- abilities need to be everpresent so we include them
          needsToBeFast =
            windowType window
              == Window.FastPlayerWindow
              && not
                ( isFastAbility ability
                    || isForced
                    || isReactionAbility ability
                )
        pure
          $ if any prevents investigatorModifiers
            || any blankPrevents modifiers'
            || needsToBeFast
            || (bountiesOnly && not sourceIsBounty)
            then Nothing
            else Just $ applyAbilityModifiers ability modifiers'

  actions''' <-
    filterM
      ( \action ->
          andM
            [ getCanPerformAbility iid window action
            , getCanAffordAbility iid action window
            ]
      )
      actions''
  forcedActions <- filterM (isForcedAbility iid) actions'''
  pure $ if null forcedActions then actions''' else forcedActions

getPlayerCountValue :: HasGame m => GameValue -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

perPlayer :: HasGame m => Int -> m Int
perPlayer = getPlayerCountValue . PerPlayer

getSpendableClueCount :: HasGame m => [InvestigatorId] -> m Int
getSpendableClueCount investigatorIds =
  getSum
    <$> selectAgg
      Sum
      InvestigatorClues
      ( Matcher.InvestigatorWithoutModifier CannotSpendClues
          <> Matcher.AnyInvestigator (map Matcher.InvestigatorWithId investigatorIds)
      )

targetToSource :: Target -> Source
targetToSource = \case
  InvestigatorTarget iid -> InvestigatorSource iid
  InvestigatorHandTarget iid -> InvestigatorSource iid
  InvestigatorDiscardTarget iid -> InvestigatorSource iid
  AssetTarget aid -> AssetSource aid
  EnemyTarget eid -> EnemySource eid
  ScenarioTarget -> ScenarioSource
  EffectTarget eid -> EffectSource eid
  PhaseTarget _ -> error "no need"
  LocationTarget lid -> LocationSource lid
  (SetAsideLocationsTarget _) -> error "can not convert"
  SkillTestTarget -> SkillTestSource
  AfterSkillTestTarget -> AfterSkillTestSource
  TreacheryTarget tid -> TreacherySource tid
  EncounterDeckTarget -> error "can not covert"
  ScenarioDeckTarget -> error "can not covert"
  AgendaTarget aid -> AgendaSource aid
  ActTarget aid -> ActSource aid
  CardIdTarget _ -> error "can not convert"
  CardCodeTarget _ -> error "can not convert"
  SearchedCardTarget _ -> error "can not convert"
  EventTarget eid -> EventSource eid
  SkillTarget sid -> SkillSource sid
  SkillTestInitiatorTarget _ -> error "can not convert"
  ChaosTokenTarget _ -> error "not convertable"
  ChaosTokenFaceTarget _ -> error "Not convertable"
  TestTarget -> TestSource mempty
  ResourceTarget -> ResourceSource
  ActDeckTarget -> ActDeckSource
  AgendaDeckTarget -> AgendaDeckSource
  InvestigationTarget {} -> error "not converted"
  YouTarget -> YouSource
  ProxyTarget {} -> error "can not convert"
  CardTarget {} -> error "can not convert"
  StoryTarget code -> StorySource code
  AgendaMatcherTarget _ -> error "can not convert"
  CampaignTarget -> CampaignSource
  TarotTarget arcana -> TarotSource arcana
  AbilityTarget _ _ -> error "can not convert"
  BothTarget t1 t2 -> BothSource (targetToSource t1) (targetToSource t2)

sourceToTarget :: Source -> Target
sourceToTarget = \case
  YouSource -> YouTarget
  AssetSource aid -> AssetTarget aid
  EnemySource eid -> EnemyTarget eid
  CardSource c -> CardTarget c
  ScenarioSource -> ScenarioTarget
  InvestigatorSource iid -> InvestigatorTarget iid
  CardCodeSource cid -> CardCodeTarget cid
  ChaosTokenSource t -> ChaosTokenTarget t
  ChaosTokenEffectSource _ -> error "not implemented"
  AgendaSource aid -> AgendaTarget aid
  LocationSource lid -> LocationTarget lid
  SkillTestSource -> SkillTestTarget
  AfterSkillTestSource -> AfterSkillTestTarget
  TreacherySource tid -> TreacheryTarget tid
  EventSource eid -> EventTarget eid
  SkillSource sid -> SkillTarget sid
  EmptyDeckSource -> error "not implemented"
  DeckSource -> error "not implemented"
  GameSource -> error "not implemented"
  ActSource aid -> ActTarget aid
  PlayerCardSource _ -> error "not implemented"
  EncounterCardSource _ -> error "not implemented"
  TestSource {} -> TestTarget
  ProxySource _ source -> sourceToTarget source
  EffectSource eid -> EffectTarget eid
  ResourceSource -> ResourceTarget
  AbilitySource {} -> error "not implemented"
  ActDeckSource -> ActDeckTarget
  AgendaDeckSource -> AgendaDeckTarget
  AgendaMatcherSource {} -> error "not converted"
  AssetMatcherSource {} -> error "not converted"
  LocationMatcherSource {} -> error "not converted"
  EnemyMatcherSource {} -> error "not converted"
  EnemyAttackSource a -> EnemyTarget a
  StorySource code -> StoryTarget code
  CampaignSource -> CampaignTarget
  TarotSource arcana -> TarotTarget arcana
  ThisCard -> error "not converted"
  CardCostSource _ -> error "not converted"
  BothSource s1 s2 -> BothTarget (sourceToTarget s1) (sourceToTarget s2)

hasFightActions
  :: HasGame m
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> [Window]
  -> m Bool
hasFightActions iid window windows' =
  anyM (\a -> anyM (\w -> getCanPerformAbility iid w a) windows')
    =<< selectList
      (Matcher.AbilityIsAction Action.Fight <> Matcher.AbilityWindow window)

hasEvadeActions
  :: HasGame m
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> [Window]
  -> m Bool
hasEvadeActions iid window windows' =
  anyM (\a -> anyM (\w -> getCanPerformAbility iid w a) windows')
    =<< selectList
      (Matcher.AbilityIsAction Action.Evade <> Matcher.AbilityWindow window)

getIsPlayable
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Source
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
  :: (HasCallStack, HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayableWithResources _ _ _ _ _ (VengeanceCard _) = pure False
getIsPlayableWithResources _ _ _ _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayableWithResources iid (toSource -> source) availableResources costStatus windows' c@(PlayerCard _) =
  withDepthGuard 3 False $ do
    iids <- filter (/= iid) <$> getInvestigatorIds
    iidsWithModifiers <- for iids $ \iid' -> do
      modifiers <- getModifiers (InvestigatorTarget iid')
      pure (iid', modifiers)
    canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) -> do
      flip anyM modifiers $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher ->
          liftA2
            (&&)
            (member iid <$> select iMatcher)
            (pure $ cardMatch c cMatcher)
        _ -> pure False
    additionalResources <-
      sum <$> traverse (field InvestigatorResources . fst) canHelpPay
    modifiers <- getModifiers (InvestigatorTarget iid)
    cardModifiers <- getModifiers (toCardId c)
    let title = nameTitle (cdName pcDef)
    passesUnique <- case (cdUnique pcDef, cdCardType pcDef) of
      (True, AssetType) ->
        not <$> case nameSubtitle (cdName pcDef) of
          Nothing -> selectAny (Matcher.AssetWithTitle title)
          Just subtitle -> selectAny (Matcher.AssetWithFullTitle title subtitle)
      _ -> pure True

    modifiedCardCost <-
      getPotentiallyModifiedCardCost iid c =<< getModifiedCardCost iid c

    let
      canAffordCost = modifiedCardCost <= (availableResources + additionalResources)
      handleCriteriaReplacement _ (CanPlayWithOverride (Criteria.CriteriaOverride cOverride)) = Just cOverride
      handleCriteriaReplacement m _ = m
      duringTurnWindow = mkWindow #when (Window.DuringTurn iid)
      notFastWindow = any (`elem` windows') [duringTurnWindow]
      canBecomeFast =
        CannotPlay Matcher.FastCard
          `notElem` modifiers
          && foldr applyModifier False modifiers
      canBecomeFastWindow =
        if canBecomeFast then Just (Matcher.DuringTurn Matcher.You) else Nothing
      applyModifier (CanBecomeFast cardMatcher) _ = cardMatch c cardMatcher
      applyModifier (CanBecomeFastOrReduceCostOf cardMatcher _) _ = canAffordCost && cardMatch c cardMatcher
      applyModifier _ val = val
    passesCriterias <-
      maybe
        (pure True)
        (passesCriteria iid (Just (c, costStatus)) (replaceThisCard c source) windows')
        (foldl' handleCriteriaReplacement (cdCriteria pcDef) cardModifiers)
    inFastWindow <-
      maybe
        (pure False)
        (cardInFastWindows iid source c windows')
        (cdFastWindow pcDef <|> canBecomeFastWindow)
    canEvade <-
      hasEvadeActions
        iid
        (Matcher.DuringTurn Matcher.You)
        windows'
    canFight <-
      hasFightActions
        iid
        (Matcher.DuringTurn Matcher.You)
        windows'
    passesLimits <- allM passesLimit (cdLimits pcDef)
    let
      additionalCosts = flip mapMaybe cardModifiers $ \case
        AdditionalCost x -> Just x
        _ -> Nothing
      sealedChaosTokenCost = flip mapMaybe (setToList $ cdKeywords pcDef) $ \case
        Keyword.Seal matcher ->
          if costStatus == PaidCost then Nothing else Just $ SealCost matcher
        _ -> Nothing

    -- Warning: We check if the source is GameSource, this affects the
    -- PlayableCardWithCostReduction matcher currently only used by Dexter
    -- Drake and De Vermis Mysteriis (2) which are non-action situations
    canAffordAdditionalCosts <-
      allM
        (getCanAffordCost iid (CardSource c) Nothing windows')
        ( [ActionCost 1 | not inFastWindow && costStatus /= PaidCost && source /= GameSource]
            <> additionalCosts
            <> sealedChaosTokenCost
        )

    passesSlots <-
      if null (cdSlots pcDef)
        then pure True
        else do
          possibleSlots <- getPotentialSlots c iid
          pure $ null $ cdSlots pcDef \\ possibleSlots

    pure
      $ (cdCardType pcDef /= SkillType)
      && ((costStatus == PaidCost) || canAffordCost)
      && none prevents modifiers
      && ((isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow)
      && ( Action.Evade
            `notElem` cdActions pcDef
            || canEvade
            || cdOverrideActionPlayableIfCriteriaMet pcDef
         )
      && ( Action.Fight
            `notElem` cdActions pcDef
            || canFight
            || cdOverrideActionPlayableIfCriteriaMet pcDef
         )
      && passesCriterias
      && passesLimits
      && passesUnique
      && passesSlots
      && canAffordAdditionalCosts
 where
  pcDef = toCardDef c
  prevents (CanOnlyUseCardsInRole role) =
    null $ intersect (cdClassSymbols pcDef) (setFromList [Neutral, role])
  prevents (CannotPlay matcher) = cardMatch c matcher
  prevents (CannotPutIntoPlay matcher) = cardMatch c matcher
  prevents _ = False
  passesLimit (LimitPerInvestigator m) = case toCardType c of
    AssetType -> do
      n <-
        selectCount
          ( Matcher.AssetControlledBy (Matcher.InvestigatorWithId iid)
              <> Matcher.AssetWithTitle (nameTitle $ toName c)
          )
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)
  passesLimit (LimitPerTrait t m) = case toCardType c of
    AssetType -> do
      n <- selectCount (Matcher.AssetWithTrait t)
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)

onSameLocation :: HasGame m => InvestigatorId -> Placement -> m Bool
onSameLocation iid = \case
  AttachedToLocation lid -> fieldMap InvestigatorLocation (== Just lid) iid
  AtLocation lid -> fieldMap InvestigatorLocation (== Just lid) iid
  InPlayArea iid' ->
    if iid == iid'
      then pure True
      else
        liftA2
          (==)
          (field InvestigatorLocation iid')
          (field InvestigatorLocation iid)
  InThreatArea iid' ->
    if iid == iid'
      then pure True
      else
        liftA2
          (==)
          (field InvestigatorLocation iid')
          (field InvestigatorLocation iid)
  AttachedToEnemy eid ->
    liftA2 (==) (field EnemyLocation eid) (field InvestigatorLocation iid)
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
  Unplaced -> pure False
  Global -> pure True
  Limbo -> pure False
  OutOfPlay _ -> pure False
  StillInHand _ -> pure False

getSpendableResources :: HasGame m => InvestigatorId -> m Int
getSpendableResources iid = do
  familyInheritanceResources <-
    getSum
      <$> selectAgg
        Sum
        AssetResources
        (Matcher.assetIs Assets.familyInheritance)
  fieldMap InvestigatorResources (+ familyInheritanceResources) iid

passesCriteria
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Maybe (Card, CostStatus)
  -> Source
  -> [Window]
  -> Criterion
  -> m Bool
passesCriteria iid mcard source windows' = \case
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
  Criteria.DuringPhase phaseMatcher -> do
    p <- getPhase
    matchPhase p phaseMatcher
  Criteria.ActionCanBeUndone -> getActionCanBeUndone
  Criteria.EncounterDeckIsNotEmpty -> do
    deck <- scenarioField ScenarioEncounterDeck
    pure $ not $ null deck
  Criteria.DoomCountIs valueMatcher -> do
    doomCount <- getDoomCount
    gameValueMatches doomCount valueMatcher
  Criteria.Negate restriction ->
    not <$> passesCriteria iid mcard source windows' restriction
  Criteria.AllUndefeatedInvestigatorsResigned ->
    andM
      [ selectNone Matcher.UneliminatedInvestigator
      , selectAny Matcher.ResignedInvestigator -- at least one investigator should have resigned
      ]
  Criteria.EachUndefeatedInvestigator investigatorMatcher -> do
    liftA2
      (==)
      (select Matcher.UneliminatedInvestigator)
      (select investigatorMatcher)
  Criteria.Never -> pure False
  Criteria.InYourHand -> do
    hand <-
      liftA2
        (<>)
        (fieldMap InvestigatorHand (map toCardId) iid)
        (map toCardId <$> getAsIfInHandCards iid)
    case source of
      EventSource eid -> do
        cardId <- field InHandEventCardId eid
        pure $ cardId `elem` hand
      AssetSource aid -> do
        inPlay <- selectAny $ Matcher.AssetWithId aid
        if inPlay
          then pure False
          else do
            -- todo we should make a cleaner method for this
            cardId <- field InHandAssetCardId aid
            pure $ cardId `elem` hand
      TreacherySource tid -> do
        member tid
          <$> select
            (Matcher.TreacheryInHandOf $ Matcher.InvestigatorWithId iid)
      _ -> error $ "source not handled for in your hand: " <> show source
  Criteria.InThreatAreaOf who -> do
    case source of
      TreacherySource tid ->
        member tid <$> select (Matcher.TreacheryInThreatAreaOf who)
      StorySource sid -> do
        placement <- field StoryPlacement sid
        case placement of
          InThreatArea iid' -> member iid' <$> select who
          _ -> pure False
      EventSource eid -> do
        placement <- field EventPlacement eid
        case placement of
          InThreatArea iid' -> member iid' <$> select who
          _ -> pure False
      _ ->
        error
          $ "Can not check if "
          <> show source
          <> " is in players threat area"
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
    _ -> pure False
  Criteria.Here -> case source of
    LocationSource lid -> fieldP InvestigatorLocation (== Just lid) iid
    ProxySource (LocationSource lid) _ ->
      fieldP InvestigatorLocation (== Just lid) iid
    _ -> pure False
  Criteria.HasSupply s -> fieldP InvestigatorSupplies (elem s) iid
  Criteria.ControlsThis -> case source of
    AssetSource aid ->
      member aid
        <$> select (Matcher.AssetControlledBy $ Matcher.InvestigatorWithId iid)
    EventSource eid ->
      member eid
        <$> select (Matcher.EventControlledBy $ Matcher.InvestigatorWithId iid)
    _ -> pure False
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
    ProxySource (AssetSource aid) _ -> fieldP AssetController isNothing aid
    _ -> error $ "missing ControlsThis check for source: " <> show source
  Criteria.OnSameLocation -> case source of
    StorySource sid -> do
      placement <- field StoryPlacement sid
      onSameLocation iid placement
    AssetSource aid -> do
      placement <- field AssetPlacement aid
      onSameLocation iid placement
    EnemySource eid -> do
      placement <- field EnemyPlacement eid
      case placement of
        Global -> pure True
        _ ->
          liftA2
            (==)
            (selectOne $ Matcher.LocationWithEnemy $ Matcher.EnemyWithId eid)
            (field InvestigatorLocation iid)
    TreacherySource tid ->
      field TreacheryAttachedTarget tid >>= \case
        Just (LocationTarget lid) ->
          fieldP InvestigatorLocation (== Just lid) iid
        Just (InvestigatorTarget iid') ->
          if iid == iid'
            then pure True
            else do
              l1 <- field InvestigatorLocation iid
              l2 <- field InvestigatorLocation iid'
              pure $ isJust l1 && l1 == l2
        Just _ -> pure False
        Nothing -> pure False
    ProxySource (AssetSource aid) _ ->
      liftA2 (==) (field AssetLocation aid) (field InvestigatorLocation iid)
    _ -> error $ "missing OnSameLocation check for source: " <> show source
  Criteria.DuringTurn who -> selectAny (Matcher.TurnInvestigator <> who)
  Criteria.CardExists cardMatcher -> selectAny cardMatcher
  Criteria.ExtendedCardExists cardMatcher -> selectAny cardMatcher
  Criteria.CommitedCardsMatch cardListMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just st -> cardListMatches (concat $ toList (skillTestCommittedCards st)) cardListMatcher
  Criteria.PlayableCardExistsWithCostReduction n cardMatcher -> do
    mTurnInvestigator <- selectOne Matcher.TurnInvestigator
    let
      updatedWindows = case mTurnInvestigator of
        Nothing -> windows'
        Just tIid ->
          nub $ mkWindow #when (Window.DuringTurn tIid) : windows'
    availableResources <- getSpendableResources iid
    results <- selectList cardMatcher
    anyM
      ( getIsPlayableWithResources
          iid
          source
          (availableResources + n)
          UnpaidCost
          updatedWindows
      )
      results
  Criteria.PlayableCardExists costStatus cardMatcher -> do
    mTurnInvestigator <- selectOne Matcher.TurnInvestigator
    let
      updatedWindows = case mTurnInvestigator of
        Nothing -> windows'
        Just tIid ->
          nub $ mkWindow #when (Window.DuringTurn tIid) : windows'
    results <- selectList cardMatcher
    anyM (getIsPlayable iid source costStatus updatedWindows) results
  Criteria.PlayableCardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
      windows'' =
        [ mkWindow #when (Window.DuringTurn iid)
        , mkWindow #when Window.FastPlayerWindow
        ]
    investigatorIds <-
      filterM
        ( fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers
            . InvestigatorTarget
        )
        =<< selectList investigatorMatcher
    discards <-
      filter (`cardMatch` cardMatcher)
        <$> concatMapM (field InvestigatorDiscard) investigatorIds
    anyM (getIsPlayable iid source UnpaidCost windows'' . PlayerCard) discards
  Criteria.FirstAction -> fieldP InvestigatorActionsTaken null iid
  Criteria.NoRestriction -> pure True
  Criteria.OnLocation locationMatcher -> do
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
    investigatorIds <-
      filterM
        ( fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers
            . InvestigatorTarget
        )
        =<< selectList investigatorMatcher
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  Criteria.CanAffordCostIncrease n -> case mcard of
    Just (card, UnpaidCost) -> do
      cost <- getModifiedCardCost iid card
      resources <- getSpendableResources iid
      pure $ resources >= cost + n
    Just (_, PaidCost) -> pure True
    Nothing -> error "no card for CanAffordCostIncrease"
  Criteria.CardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- selectList investigatorMatcher
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let filteredDiscards = filter (`cardMatch` cardMatcher) discards
    pure $ notNull filteredDiscards
  Criteria.ClueOnLocation ->
    maybe (pure False) (fieldP LocationClues (> 0))
      =<< field InvestigatorLocation iid
  Criteria.EnemyCriteria enemyCriteria ->
    passesEnemyCriteria iid source windows' enemyCriteria
  Criteria.SetAsideCardExists matcher -> selectAny matcher
  Criteria.OutOfPlayEnemyExists outOfPlayZone matcher ->
    selectAny $ Matcher.OutOfPlayEnemy outOfPlayZone matcher
  Criteria.OnAct step -> do
    actId <- selectJust Matcher.AnyAct
    (== AS.ActStep step) . AS.actStep <$> field ActSequence actId
  Criteria.AgendaExists matcher -> selectAny matcher
  Criteria.ActExists matcher -> selectAny matcher
  Criteria.AssetExists matcher -> do
    selectAny (Matcher.resolveAssetMatcher iid matcher)
  Criteria.EventExists matcher -> do
    selectAny (Matcher.resolveEventMatcher iid matcher)
  Criteria.ExcludeWindowAssetExists matcher -> case getWindowAsset windows' of
    Nothing -> pure False
    Just aid -> do
      selectAny
        $ Matcher.NotAsset (Matcher.AssetWithId aid)
        <> Matcher.resolveAssetMatcher iid matcher
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
  Criteria.Criteria rs -> allM (passesCriteria iid mcard source windows') rs
  Criteria.AnyCriterion rs -> anyM (passesCriteria iid mcard source windows') rs
  Criteria.LocationExists matcher -> do
    selectAny (Matcher.replaceYourLocation iid matcher)
  Criteria.LocationCount n matcher -> do
    (== n) <$> selectCount (Matcher.replaceYourLocation iid matcher)
  Criteria.ExtendedCardCount n matcher ->
    (== n) <$> selectCount matcher
  Criteria.AllLocationsMatch targetMatcher locationMatcher -> do
    targets <- select (Matcher.replaceYourLocation iid targetMatcher)
    actual <- select (Matcher.replaceYourLocation iid locationMatcher)
    pure $ all (`member` actual) targets
  Criteria.InvestigatorIsAlone ->
    (== 1) <$> selectCount (Matcher.colocatedWith iid)
  Criteria.InVictoryDisplay cardMatcher valueMatcher -> do
    vCards <-
      filter (`cardMatch` cardMatcher)
        <$> scenarioField ScenarioVictoryDisplay
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
        ActionAbility (Just Action.Resign) _ -> True
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
    m <- countM (passesCriteria iid mcard source windows') criteria
    pure $ m >= n
  Criteria.DuringAction -> case mcard of
    Just (_, PaidCost) -> pure False -- If the cost is paid we're in a play action so we have to assume it is always False or it will never trigger
    _ -> getGameInAction
  Criteria.AffectedByTarot -> case source of
    TarotSource card -> affectedByTarot iid card
    _ -> pure False

getWindowAsset :: [Window] -> Maybe AssetId
getWindowAsset [] = Nothing
getWindowAsset ((windowType -> Window.ActivateAbility _ ability) : xs) = case abilitySource ability of
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
  selectAny . matcherF =<< matcher criterion
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

getModifiedCardCost :: HasGame m => InvestigatorId -> Card -> m Int
getModifiedCardCost iid c@(PlayerCard _) = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  cardModifiers <- getModifiers (CardIdTarget $ toCardId c)
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  pcDef = toCardDef c
  startingCost = case cdCost pcDef of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0
  -- A card like The Painted World which has no cost, but can be "played", should not have it's cost modified
  applyModifier n _ | isNothing (cdCost pcDef) = pure n
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then n + m else n
  applyModifier n _ = pure n
getModifiedCardCost iid c@(EncounterCard _) = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then n + m else n
  applyModifier n _ = pure n
getModifiedCardCost _ (VengeanceCard _) =
  error "should not happen for vengeance"

getPotentiallyModifiedCardCost
  :: HasGame m => InvestigatorId -> Card -> Int -> m Int
getPotentiallyModifiedCardCost iid c@(PlayerCard _) startingCost = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  cardModifiers <- getModifiers (CardIdTarget $ toCardId c)
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  applyModifier n (CanReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (CanBecomeFastOrReduceCostOf cardMatcher m) = do
    -- get is playable will check if this has to be used, will likely break if
    -- anything else aside from Chuck Fergus (2) interacts with this
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n _ = pure n
getPotentiallyModifiedCardCost iid c@(EncounterCard _) _ = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (CanReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (CanBecomeFastOrReduceCostOf cardMatcher m) = do
    -- get is playable will check if this has to be used
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n _ = pure n
getPotentiallyModifiedCardCost _ (VengeanceCard _) _ =
  error "should not check vengeance card"

cardInFastWindows
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Card
  -> [Window]
  -> Matcher.WindowMatcher
  -> m Bool
cardInFastWindows iid source _ windows' matcher =
  anyM (\window -> windowMatches iid source window matcher) windows'

windowMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Window
  -> Matcher.WindowMatcher
  -> m Bool
windowMatches _ _ (windowType -> Window.DoNotCheckWindow) _ = pure True
windowMatches iid source window'@(windowTiming &&& windowType -> (timing', wType)) mtchr = do
  let noMatch = pure False
  let isMatch = pure True
  let guardTiming t body = if timing' == t then body wType else noMatch
  case mtchr of
    Matcher.NotAnyWindow -> noMatch
    Matcher.AnyWindow -> isMatch
    Matcher.WouldSearchDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.WouldSearchDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch iid deck
              $ over biplate (Matcher.replaceInvestigatorMatcher who Matcher.ThatInvestigator) deckMatcher
          ]
      _ -> noMatch
    Matcher.SearchedDeck timing whoMatcher deckMatcher -> guardTiming timing $ \case
      Window.SearchedDeck who deck -> do
        andM
          [ matchWho iid who whoMatcher
          , deckMatch iid deck
              $ over biplate (Matcher.replaceInvestigatorMatcher who Matcher.ThatInvestigator) deckMatcher
          ]
      _ -> noMatch
    Matcher.WouldTriggerChaosTokenRevealEffectOnCard whoMatcher cardMatcher tokens ->
      guardTiming Timing.AtIf $ \case
        Window.RevealChaosTokenEffect who token effectId -> do
          cardCode <- field EffectCardCode effectId
          andM
            [ matchWho iid who whoMatcher
            , pure $ chaosTokenFace token `elem` tokens
            , pure $ lookupCard cardCode nullCardId `cardMatch` cardMatcher
            ]
        Window.RevealChaosTokenEventEffect who tokens' eventId -> do
          card <- field EventCard eventId
          andM
            [ matchWho iid who whoMatcher
            , pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            ]
        Window.RevealChaosTokenAssetAbilityEffect who tokens' assetId -> do
          card <- field AssetCard assetId
          andM
            [ matchWho iid who whoMatcher
            , pure $ any ((`elem` tokens) . chaosTokenFace) tokens'
            , pure $ card `cardMatch` cardMatcher
            ]
        _ -> noMatch
    Matcher.GameBegins timing -> guardTiming timing $ pure . (== Window.GameBegins)
    Matcher.InvestigatorTakeDamage timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeDamage source' _ (InvestigatorTarget who) ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorTakeHorror timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeHorror source' (InvestigatorTarget who) ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldTakeDamage timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.WouldTakeDamage source' (InvestigatorTarget who) _ ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        Window.WouldTakeDamageOrHorror source' (InvestigatorTarget who) n _ | n > 0 -> do
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.InvestigatorWouldTakeHorror timing whoMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.WouldTakeHorror source' (InvestigatorTarget who) _ ->
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
            ]
        Window.WouldTakeDamageOrHorror source' (InvestigatorTarget who) _ n | n > 0 -> do
          andM
            [ sourceMatches source' sourceMatcher
            , matchWho iid who whoMatcher
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
          [ sourceMatches source' sourceMatcher
          , matchWho iid who whoMatcher
          ]
      _ -> noMatch
    Matcher.LostResources timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.LostResources who source' _ ->
        andM
          [ sourceMatches source' sourceMatcher
          , matchWho iid who whoMatcher
          ]
      _ -> noMatch
    Matcher.CancelledOrIgnoredCardOrGameEffect sourceMatcher ->
      guardTiming Timing.After $ \case
        Window.CancelledOrIgnoredCardOrGameEffect source' ->
          sourceMatches source' sourceMatcher
        _ -> noMatch
    Matcher.WouldBeShuffledIntoDeck deckMatcher cardMatcher -> case wType of
      Window.WouldBeShuffledIntoDeck deck card ->
        andM [deckMatch iid deck deckMatcher, pure $ cardMatch card cardMatcher]
      _ -> noMatch
    Matcher.AddingToCurrentDepth -> case wType of
      Window.AddingToCurrentDepth -> isMatch
      _ -> noMatch
    Matcher.DrawingStartingHand timing whoMatcher -> guardTiming timing $ \case
      Window.DrawingStartingHand who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.MovedFromHunter timing enemyMatcher -> guardTiming timing $ \case
      Window.MovedFromHunter eid -> member eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.EnemyMovedTo timing locationMatcher movesVia enemyMatcher -> guardTiming timing $ \case
      Window.EnemyMovesTo lid movesVia' eid
        | movesVia == movesVia' ->
            andM [member eid <$> select enemyMatcher, member lid <$> select locationMatcher]
      _ -> noMatch
    Matcher.PlaceUnderneath timing targetMatcher cardMatcher -> guardTiming timing $ \case
      Window.PlaceUnderneath target' card ->
        andM
          [ targetMatches target' targetMatcher
          , pure $ cardMatch card cardMatcher
          ]
      _ -> noMatch
    Matcher.ActivateAbility timing whoMatcher abilityMatcher -> guardTiming timing $ \case
      Window.ActivateAbility who ability ->
        andM
          [ matchWho iid who whoMatcher
          , member ability <$> select abilityMatcher
          ]
      _ -> noMatch
    Matcher.CommittedCard timing whoMatcher cardMatcher -> guardTiming timing $ \case
      Window.CommittedCard who card ->
        andM
          [ matchWho iid who whoMatcher
          , pure $ cardMatch card cardMatcher
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
            _ -> noMatch -- TODO: We may need more things here
        _ -> noMatch
    Matcher.TookControlOfAsset timing whoMatcher assetMatcher ->
      guardTiming timing $ \case
        Window.TookControlOfAsset who aid ->
          andM
            [ matchWho iid who whoMatcher
            , member aid <$> select assetMatcher
            ]
        _ -> noMatch
    Matcher.AssetHealed timing damageType assetMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.Healed damageType' (AssetTarget assetId) source' _ | damageType == damageType' -> do
          andM
            [ member assetId <$> select assetMatcher
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
        Window.WouldPerformRevelationSkillTest who -> matchWho iid who whoMatcher
        _ -> noMatch
    Matcher.WouldDrawEncounterCard timing whoMatcher phaseMatcher ->
      guardTiming timing $ \case
        Window.WouldDrawEncounterCard who p ->
          andM [matchWho iid who whoMatcher, matchPhase p phaseMatcher]
        _ -> noMatch
    Matcher.AmongSearchedCards whoMatcher -> case wType of
      Window.AmongSearchedCards _ who -> matchWho iid who whoMatcher
      _ -> noMatch
    Matcher.Discarded timing whoMatcher sourceMatcher cardMatcher ->
      guardTiming timing $ \case
        Window.Discarded who source' card ->
          andM
            [ pure $ cardMatch card cardMatcher
            , matchWho iid who whoMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.AssetWouldBeDiscarded timing assetMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (AssetTarget aid) -> elem aid <$> select assetMatcher
      _ -> noMatch
    Matcher.EnemyWouldBeDiscarded timing enemyMatcher -> guardTiming timing $ \case
      Window.WouldBeDiscarded (EnemyTarget eid) -> elem eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.AgendaAdvances timing agendaMatcher -> guardTiming timing $ \case
      Window.AgendaAdvance aid -> agendaMatches aid agendaMatcher
      _ -> noMatch
    Matcher.MovedBy timing whoMatcher sourceMatcher -> guardTiming timing $ \case
      Window.MovedBy source' _ who ->
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
      Window.InvestigatorEliminated who -> matchWho iid who (Matcher.IncludeEliminated whoMatcher)
      _ -> noMatch
    Matcher.PutLocationIntoPlay timing whoMatcher locationMatcher ->
      guardTiming timing $ \case
        Window.PutLocationIntoPlay who locationId ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' locationId locationMatcher
            ]
        _ -> noMatch
    Matcher.PlayerHasPlayableCard cardMatcher -> do
      -- TODO: do we need to grab the card source?
      -- cards <- filter (/= c) <$> getList cardMatcher
      cards <- selectList cardMatcher
      anyM (getIsPlayable iid source UnpaidCost [window']) cards
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
        Window.Moves iid' source' mFromLid toLid ->
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
        andM [matchWho iid iid' whoMatcher, actionMatches action actionMatcher]
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
    Matcher.InitiatedSkillTest timing whoMatcher skillTypeMatcher skillValueMatcher ->
      guardTiming timing $ \case
        Window.InitiatedSkillTest st -> case skillTestType st of
          SkillSkillTest skillType | skillTypeMatches skillType skillTypeMatcher -> do
            andM
              [ matchWho iid (skillTestInvestigator st) whoMatcher
              , skillTestValueMatches
                  iid
                  (skillTestDifficulty st)
                  (skillTestAction st)
                  (skillTestType st)
                  skillValueMatcher
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
                  Window.FailSkillTest who n ->
                    andM
                      [ matchWho iid who whoMatcher
                      , gameValueMatches n gameValueMatcher
                      ]
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
                  Window.SuccessfulAttackEnemy who enemyId n -> case skillMatcher of
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
                  Window.PassSkillTest _ _ who n ->
                    andM
                      [ matchWho iid who whoMatcher
                      , gameValueMatches n gameValueMatcher
                      ]
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
        pure $ Just iid == miid
      _ -> noMatch
    Matcher.OrWindowMatcher matchers ->
      anyM (windowMatches iid source window') matchers
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
              , enemyAttackMatches details enemyAttackMatcher
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
              , enemyAttackMatches details enemyAttackMatcher
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
              , enemyAttackMatches details enemyAttackMatcher
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
    Matcher.EnemyAttackedSuccessfully timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.SuccessfulAttackEnemy who enemyId _ -> do
          andM
            [ enemyMatches enemyId enemyMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.EnemyEvaded timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEvaded who enemyId ->
          andM
            [ enemyMatches enemyId enemyMatcher
            , matchWho iid who whoMatcher
            ]
        _ -> noMatch
    Matcher.EnemyEngaged timing whoMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyEngaged who enemyId ->
          andM
            [ enemyMatches enemyId enemyMatcher
            , matchWho iid who whoMatcher
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
            [ member assetId <$> select assetMatcher
            , defeatedByMatches defeatedBy defeatedByMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDefeated timing whoMatcher defeatedByMatcher enemyMatcher ->
      guardTiming timing $ \case
        Window.EnemyDefeated (Just who) defeatedBy enemyId ->
          andM
            [ enemyMatches enemyId enemyMatcher
            , matchWho iid who whoMatcher
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
    Matcher.FastPlayerWindow -> guardTiming #when (pure . (== Window.FastPlayerWindow))
    Matcher.DealtDamageOrHorror timing sourceMatcher whoMatcher ->
      case whoMatcher of
        Matcher.You -> guardTiming timing $ \case
          Window.WouldTakeDamageOrHorror source' (InvestigatorTarget iid') _ _ ->
            andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
          Window.WouldTakeDamageOrHorror source' (AssetTarget aid) _ _ ->
            andM
              [ member aid
                  <$> select
                    ( Matcher.AssetControlledBy
                        $ Matcher.replaceYouMatcher iid whoMatcher
                    )
              , sourceMatches source' sourceMatcher
              ]
          _ -> noMatch
        _ -> noMatch
    Matcher.DealtDamage timing sourceMatcher whoMatcher -> guardTiming timing $ \case
      Window.DealtDamage source' _ (InvestigatorTarget iid') _ ->
        andM [matchWho iid iid' whoMatcher, sourceMatches source' sourceMatcher]
      Window.DealtDamage source' _ (AssetTarget aid) _ ->
        andM
          [ member aid
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
          [ member aid
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
            [ member aid <$> select assetMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDealtDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.DealtDamage source' damageEffect (EnemyTarget eid) _ ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , member eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyDealtExcessDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.DealtExcessDamage source' damageEffect (EnemyTarget eid) _ ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , member eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.EnemyTakeDamage timing damageEffectMatcher enemyMatcher sourceMatcher ->
      guardTiming timing $ \case
        Window.TakeDamage source' damageEffect (EnemyTarget eid) ->
          andM
            [ damageEffectMatches damageEffect damageEffectMatcher
            , member eid <$> select enemyMatcher
            , sourceMatches source' sourceMatcher
            ]
        _ -> noMatch
    Matcher.DiscoverClues timing whoMatcher whereMatcher valueMatcher ->
      guardTiming timing $ \case
        Window.DiscoverClues who lid _ n ->
          andM
            [ matchWho iid who whoMatcher
            , locationMatches iid source window' lid whereMatcher
            , gameValueMatches n valueMatcher
            ]
        _ -> noMatch
    Matcher.GainsClues timing whoMatcher valueMatcher -> guardTiming timing $ \case
      Window.GainsClues who _ n ->
        andM [matchWho iid who whoMatcher, gameValueMatches n valueMatcher]
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
      Window.LastClueRemovedFromAsset aid -> member aid <$> select assetMatcher
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
                _ -> member card <$> select cardMatcher
            , deckMatch iid deck deckMatcher
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
      Window.PlayCard who card ->
        andM
          [ matchWho iid who whoMatcher
          , case cardMatcher of
              Matcher.BasicCardMatch baseMatcher ->
                pure $ cardMatch card baseMatcher
              _ -> member card <$> select cardMatcher
          ]
      _ -> noMatch
    Matcher.AgendaEntersPlay timing agendaMatcher -> guardTiming timing $ \case
      Window.EnterPlay (AgendaTarget aid) -> member aid <$> select agendaMatcher
      _ -> noMatch
    Matcher.AssetEntersPlay timing assetMatcher -> guardTiming timing $ \case
      Window.EnterPlay (AssetTarget aid) -> member aid <$> select assetMatcher
      _ -> noMatch
    Matcher.AssetLeavesPlay timing assetMatcher -> guardTiming timing $ \case
      Window.LeavePlay (AssetTarget aid) -> member aid <$> select assetMatcher
      _ -> noMatch
    Matcher.EnemyEntersPlay timing enemyMatcher -> guardTiming timing $ \case
      Window.EnterPlay (EnemyTarget eid) -> member eid <$> select enemyMatcher
      _ -> noMatch
    Matcher.LocationLeavesPlay timing locationMatcher -> guardTiming timing $ \case
      Window.LeavePlay (LocationTarget aid) -> member aid <$> select locationMatcher
      _ -> noMatch
    Matcher.EnemyLeavesPlay timing enemyMatcher -> guardTiming timing $ \case
      Window.LeavePlay (EnemyTarget eid) -> member eid <$> select enemyMatcher
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
  who <=~> Matcher.InvestigatorAt (Matcher.replaceYourLocation iid matcher)
matchWho iid who matcher =
  member who <$> (select =<< replaceMatchWhoLocations iid matcher)
 where
  replaceMatchWhoLocations iid' = \case
    Matcher.InvestigatorAt matcher' -> do
      pure $ Matcher.InvestigatorAt $ Matcher.replaceYourLocation iid matcher'
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
  -> Int
  -> Maybe Action
  -> SkillTestType
  -> Matcher.SkillTestValueMatcher
  -> m Bool
skillTestValueMatches iid n maction skillTestType = \case
  Matcher.AnySkillTestValue -> pure True
  Matcher.SkillTestGameValue valueMatcher -> gameValueMatches n valueMatcher
  Matcher.GreaterThanBaseValue -> case skillTestType of
    SkillSkillTest skillType -> do
      baseSkill <- baseSkillValueFor skillType maction [] iid
      pure $ n > baseSkill
    AndSkillTest types -> do
      baseSkill <- sum <$> traverse (\skillType -> baseSkillValueFor skillType maction [] iid) types
      pure $ n > baseSkill
    ResourceSkillTest -> do
      resources <- field InvestigatorResources iid
      pure $ n > resources

targetTraits :: (HasCallStack, HasGame m) => Target -> m (Set Trait)
targetTraits = \case
  ActDeckTarget -> pure mempty
  ActTarget _ -> pure mempty
  AfterSkillTestTarget -> pure mempty
  AgendaDeckTarget -> pure mempty
  AgendaTarget _ -> pure mempty
  AssetTarget aid -> field AssetTraits aid
  CardCodeTarget _ -> pure mempty
  CardIdTarget _ -> pure mempty
  EffectTarget _ -> pure mempty
  EnemyTarget eid -> field EnemyTraits eid
  EventTarget eid -> field EventTraits eid
  InvestigatorTarget iid -> field InvestigatorTraits iid
  LocationTarget lid ->
    selectOne (Matcher.LocationWithId lid) >>= \case
      Nothing -> pure mempty
      Just _ -> field LocationTraits lid
  ProxyTarget t _ -> targetTraits t
  ResourceTarget -> pure mempty
  ScenarioTarget -> pure mempty
  SkillTarget sid -> field SkillTraits sid
  SkillTestTarget {} -> pure mempty
  TreacheryTarget tid -> field TreacheryTraits tid
  StoryTarget _ -> pure mempty
  TestTarget -> pure mempty
  ChaosTokenTarget _ -> pure mempty
  YouTarget -> selectJust Matcher.You >>= field InvestigatorTraits
  InvestigatorHandTarget _ -> pure mempty
  InvestigatorDiscardTarget _ -> pure mempty
  SetAsideLocationsTarget _ -> pure mempty
  EncounterDeckTarget -> pure mempty
  ScenarioDeckTarget -> pure mempty
  CardTarget c -> pure $ toTraits c
  SearchedCardTarget _ -> pure mempty
  SkillTestInitiatorTarget _ -> pure mempty
  PhaseTarget _ -> pure mempty
  ChaosTokenFaceTarget _ -> pure mempty
  InvestigationTarget _ _ -> pure mempty
  AgendaMatcherTarget _ -> pure mempty
  CampaignTarget -> pure mempty
  TarotTarget _ -> pure mempty
  AbilityTarget _ _ -> pure mempty
  BothTarget _ _ -> error "won't make sense, or need to determine later"

targetMatches :: HasGame m => Target -> Matcher.TargetMatcher -> m Bool
targetMatches s = \case
  Matcher.TargetMatchesAny ms -> anyM (targetMatches s) ms
  Matcher.TargetIs s' -> pure $ s == s'
  Matcher.AnyTarget -> pure True
  Matcher.TargetMatches ms -> allM (targetMatches s) ms
  Matcher.LocationTargetMatches locationMatcher -> case s of
    LocationTarget lid -> lid <=~> locationMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (Matcher.LocationTargetMatches locationMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (Matcher.LocationTargetMatches locationMatcher)
        , targetMatches right (Matcher.LocationTargetMatches locationMatcher)
        ]
    _ -> pure False
  Matcher.ActTargetMatches actMatcher -> case s of
    ActTarget aid -> aid <=~> actMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (Matcher.ActTargetMatches actMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (Matcher.ActTargetMatches actMatcher)
        , targetMatches right (Matcher.ActTargetMatches actMatcher)
        ]
    _ -> pure False
  Matcher.AgendaTargetMatches agendaMatcher -> case s of
    AgendaTarget aid -> aid <=~> agendaMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (Matcher.AgendaTargetMatches agendaMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (Matcher.AgendaTargetMatches agendaMatcher)
        , targetMatches right (Matcher.AgendaTargetMatches agendaMatcher)
        ]
    _ -> pure False
  Matcher.ScenarioCardTarget -> case s of
    EnemyTarget _ -> pure True
    TreacheryTarget _ -> pure True
    AgendaTarget _ -> pure True
    ActTarget _ -> pure True
    LocationTarget _ -> pure True
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget Matcher.ScenarioCardTarget
    BothTarget left right ->
      orM [targetMatches left Matcher.ScenarioCardTarget, targetMatches right Matcher.ScenarioCardTarget]
    _ -> pure False

enemyMatches :: HasGame m => EnemyId -> Matcher.EnemyMatcher -> m Bool
enemyMatches !enemyId !mtchr = member enemyId <$> select mtchr

locationMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Window
  -> LocationId
  -> Matcher.LocationMatcher
  -> m Bool
locationMatches investigatorId source window locationId matcher' = do
  let matcher = Matcher.replaceYourLocation investigatorId matcher'
  case matcher of
    -- special cases
    Matcher.NotLocation m -> not <$> locationMatches investigatorId source window locationId m
    Matcher.IncludeEmptySpace _ -> locationId <=~> matcher
    Matcher.LocationCanBeEnteredBy {} -> locationId <=~> matcher
    Matcher.MostBreaches _ -> locationId <=~> matcher
    Matcher.FewestBreaches {} -> locationId <=~> matcher
    Matcher.LocationWithBreaches _ -> locationId <=~> matcher
    Matcher.LocationWithBrazier _ -> locationId <=~> matcher
    Matcher.LocationWithIncursion -> locationId <=~> matcher
    Matcher.LocationWithDefeatedEnemyThisRound -> locationId <=~> matcher
    Matcher.LocationWithDiscoverableCluesBy _ -> locationId <=~> matcher
    Matcher.LocationWithoutModifier _ -> locationId <=~> matcher
    Matcher.LocationWithModifier _ -> locationId <=~> matcher
    Matcher.IsIchtacasDestination -> locationId <=~> matcher
    Matcher.HauntedLocation -> locationId <=~> matcher
    Matcher.SingleSidedLocation -> locationId <=~> matcher
    Matcher.LocationWithEnemy enemyMatcher -> selectAny $ Matcher.enemyAt locationId <> enemyMatcher
    Matcher.LocationWithAsset assetMatcher -> selectAny $ Matcher.assetAt locationId <> assetMatcher
    Matcher.LocationWithInvestigator whoMatcher -> selectAny $ Matcher.investigatorAt locationId <> whoMatcher
    Matcher.LocationWithoutTreachery treacheryMatcher -> do selectNone $ Matcher.treacheryAt locationId <> treacheryMatcher
    Matcher.LocationWithTreachery treacheryMatcher -> do selectAny $ Matcher.treacheryAt locationId <> treacheryMatcher

    -- normal cases
    Matcher.LocationWithLowerShroudThan _ -> locationId <=~> matcher
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
    Matcher.LocationWithTrait _ -> locationId <=~> matcher
    Matcher.LocationWithoutTrait _ -> locationId <=~> matcher
    Matcher.LocationInDirection _ _ -> locationId <=~> matcher
    Matcher.ClosestPathLocation _ _ -> locationId <=~> matcher
    Matcher.LocationWithoutClues -> locationId <=~> matcher
    Matcher.HighestShroud _ -> locationId <=~> matcher
    Matcher.LocationWithDistanceFrom _ _ -> locationId <=~> matcher
    Matcher.LocationWithClues valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues locationId
    Matcher.LocationWithDoom valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationDoom locationId
    Matcher.LocationWithHorror valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationHorror locationId
    Matcher.LocationWithMostClues locationMatcher ->
      member locationId
        <$> select (Matcher.LocationWithMostClues locationMatcher)
    Matcher.LocationWithResources valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationResources locationId
    Matcher.LocationLeavingPlay -> case windowType window of
      Window.LeavePlay (LocationTarget lid) ->
        pure $ locationId == lid
      _ -> error "invalid window for LocationLeavingPlay"
    Matcher.SameLocation -> do
      mlid' <- case source of
        EnemySource eid -> field EnemyLocation eid
        AssetSource aid -> field AssetLocation aid
        _ -> error $ "can't detect same location for source " <> show source
      pure $ Just locationId == mlid'
    Matcher.YourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId == yourLocationId
    Matcher.ThisLocation -> case source of
      (LocationSource lid) -> pure $ lid == locationId
      (ProxySource (LocationSource lid) _) -> pure $ lid == locationId
      _ -> error "Invalid source for ThisLocation"
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

skillTestMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestMatches iid source st = \case
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
  Matcher.UsingThis -> pure $ source == skillTestSource st
  Matcher.SkillTestSourceMatches sourceMatcher ->
    sourceMatches (skillTestSource st) sourceMatcher
  Matcher.SkillTestFromRevelation -> pure $ skillTestIsRevelation st
  Matcher.SkillTestForAction actionMatcher -> case skillTestAction st of
    Just action -> action `actionMatches` actionMatcher
    Nothing -> pure False
  Matcher.WhileInvestigating locationMatcher -> case skillTestAction st of
    Just Action.Investigate -> case skillTestTarget st of
      LocationTarget lid -> member lid <$> select locationMatcher
      ProxyTarget (LocationTarget lid) _ ->
        member lid <$> select locationMatcher
      _ -> pure False
    _ -> pure False
  Matcher.SkillTestOnTreachery treacheryMatcher -> case skillTestSource st of
    TreacherySource tid -> member tid <$> select treacheryMatcher
    _ -> pure False
  Matcher.WhileAttackingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Fight -> case skillTestTarget st of
      EnemyTarget eid -> member eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.WhileEvadingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Evade -> case skillTestTarget st of
      EnemyTarget eid -> member eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.SkillTestWithSkill sk -> selectAny sk
  Matcher.SkillTestWithSkillType sType -> pure $ case skillTestType st of
    SkillSkillTest sType' -> sType' == sType
    AndSkillTest types -> sType `elem` types
    ResourceSkillTest -> False
  Matcher.SkillTestAtYourLocation -> do
    mlid1 <- field InvestigatorLocation iid
    mlid2 <- field InvestigatorLocation $ skillTestInvestigator st
    case (mlid1, mlid2) of
      (Just lid1, Just lid2) -> pure $ lid1 == lid2
      _ -> pure False
  Matcher.SkillTestMatches ms -> allM (skillTestMatches iid source st) ms

matchChaosToken
  :: HasGame m => InvestigatorId -> ChaosToken -> Matcher.ChaosTokenMatcher -> m Bool
matchChaosToken _ = (<=~>)

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
  Matcher.AnyCards -> pure True
  Matcher.LengthIs valueMatcher -> gameValueMatches (length cards) valueMatcher
  Matcher.DifferentLengthIsAtLeast n cardMatcher -> pure $ length (nubOrdOn toTitle $ filter (`cardMatch` cardMatcher) cards) >= n
  Matcher.HasCard cardMatcher -> pure $ any (`cardMatch` cardMatcher) cards

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
  Matcher.DeckOf investigatorMatcher -> matchWho iid iid investigatorMatcher
  Matcher.AnyDeck -> pure True
  Matcher.DeckIs deckSignifier' -> pure $ deckSignifier == deckSignifier'
  Matcher.DeckOneOf matchers' -> anyM (deckMatch iid deckSignifier) matchers'

agendaMatches :: HasGame m => AgendaId -> Matcher.AgendaMatcher -> m Bool
agendaMatches !agendaId !mtchr = member agendaId <$> select mtchr

actionMatches :: Monad m => Action -> Matcher.ActionMatcher -> m Bool
actionMatches _ Matcher.AnyAction = pure True
actionMatches a (Matcher.ActionIs a') = pure $ a == a'
actionMatches a (Matcher.ActionOneOf as) = anyM (actionMatches a) as

skillTypeMatches :: SkillType -> Matcher.SkillTypeMatcher -> Bool
skillTypeMatches st = \case
  Matcher.AnySkillType -> True
  Matcher.NotSkillType st' -> st /= st'
  Matcher.IsSkillType st' -> st == st'

enemyAttackMatches :: HasGame m => EnemyAttackDetails -> Matcher.EnemyAttackMatcher -> m Bool
enemyAttackMatches details@EnemyAttackDetails {..} = \case
  Matcher.AnyEnemyAttack -> pure True
  Matcher.AttackOfOpportunityAttack -> pure $ attackType == AttackOfOpportunity
  Matcher.CancelableEnemyAttack matcher -> do
    modifiers' <- getModifiers (sourceToTarget attackSource)
    enemyModifiers <- getModifiers attackEnemy
    andM
      [ enemyAttackMatches details matcher
      , pure $ EffectsCannotBeCanceled `notElem` modifiers'
      , pure $ AttacksCannotBeCancelled `notElem` enemyModifiers
      ]

damageEffectMatches
  :: Monad m => DamageEffect -> Matcher.DamageEffectMatcher -> m Bool
damageEffectMatches a = \case
  Matcher.AnyDamageEffect -> pure True
  Matcher.AttackDamageEffect -> pure $ a == AttackDamageEffect
  Matcher.NonAttackDamageEffect -> pure $ a == NonAttackDamageEffect

spawnAtOneOf :: (HasGame m, HasQueue Message m) => InvestigatorId -> EnemyId -> [LocationId] -> m ()
spawnAtOneOf iid eid targetLids = do
  locations' <- select $ Matcher.IncludeEmptySpace Matcher.Anywhere
  case setToList (setFromList targetLids `intersection` locations') of
    [] -> push (Discard GameSource (EnemyTarget eid))
    [lid] -> do
      windows' <- checkWindows [Window #when (Window.EnemyWouldSpawnAt eid lid) Nothing]
      pushAll $ windows' : resolve (EnemySpawn Nothing lid eid)
    lids -> do
      windowPairs <- for lids $ \lid -> do
        windows' <- checkWindows [Window #when (Window.EnemyWouldSpawnAt eid lid) Nothing]
        pure (windows', lid)

      push
        $ chooseOne
          iid
          [ targetLabel lid $ windows' : resolve (EnemySpawn Nothing lid eid)
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

getCanShuffleDeck :: HasGame m => InvestigatorId -> m Bool
getCanShuffleDeck iid = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  pure $ CannotManipulateDeck `notElem` modifiers

getDoomCount :: HasGame m => m Int
getDoomCount =
  getSum
    . fold
    <$> sequence
      [ selectAgg Sum AssetDoom Matcher.AnyAsset
      , selectAgg Sum EnemyDoom Matcher.AnyEnemy
      , selectAgg Sum LocationDoom Matcher.Anywhere
      , selectAgg Sum TreacheryDoom Matcher.AnyTreachery
      , selectAgg Sum AgendaDoom Matcher.AnyAgenda
      , selectAgg Sum InvestigatorDoom Matcher.UneliminatedInvestigator
      ]

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

isForcedAbility :: HasGame m => InvestigatorId -> Ability -> m Bool
isForcedAbility iid Ability {abilitySource, abilityType} = isForcedAbilityType iid abilitySource abilityType

isForcedAbilityType :: HasGame m => InvestigatorId -> Source -> AbilityType -> m Bool
isForcedAbilityType iid source = \case
  SilentForcedAbility {} -> pure True
  ForcedAbility {} -> pure True
  ForcedAbilityWithCost {} -> pure True
  Objective aType -> isForcedAbilityType iid source aType
  FastAbility' {} -> pure False
  ReactionAbility {} -> pure False
  ActionAbility {} -> pure False
  ActionAbilityWithSkill {} -> pure False
  ActionAbilityWithBefore {} -> pure False
  AbilityEffect {} -> pure False
  Haunted {} -> pure True -- Maybe? we wanted this to basically never be valid but still take forced precedence
  Cosmos {} -> pure True -- Maybe? we wanted this to basically never be valid but still take forced precedence
  ForcedWhen c _ -> passesCriteria iid Nothing source [] c

sourceMatches
  :: (HasCallStack, HasGame m) => Source -> Matcher.SourceMatcher -> m Bool
sourceMatches s = \case
  Matcher.SourceIsCancelable sm -> case s of
    CardCostSource _ -> pure False
    other -> do
      modifiers' <- getModifiers (sourceToTarget other)
      andM [sourceMatches s sm, pure $ EffectsCannotBeCanceled `notElem` modifiers']
  Matcher.SourceIs s' -> pure $ s == s'
  Matcher.NotSource matcher -> not <$> sourceMatches s matcher
  Matcher.SourceMatchesAny ms -> anyM (sourceMatches s) ms
  Matcher.SourceWithTrait t -> elem t <$> sourceTraits s
  Matcher.SourceIsEnemyAttack em -> case s of
    EnemyAttackSource eid -> member eid <$> select em
    _ -> pure False
  Matcher.AnySource -> pure True
  Matcher.SourceMatches ms -> allM (sourceMatches s) ms
  Matcher.SourceOwnedBy whoMatcher ->
    let
      checkSource = \case
        AbilitySource source' _ -> checkSource source'
        AssetSource aid -> do
          mControllerId <- selectAssetController aid
          case mControllerId of
            Just iid' -> member iid' <$> select whoMatcher
            _ -> pure False
        EventSource eid -> do
          mControllerId <- selectEventController eid
          case mControllerId of
            Just controllerId -> member controllerId <$> select whoMatcher
            Nothing -> pure False
        SkillSource sid -> do
          mControllerId <- selectSkillController sid
          case mControllerId of
            Just controllerId -> member controllerId <$> select whoMatcher
            Nothing -> pure False
        InvestigatorSource iid -> member iid <$> select whoMatcher
        CardSource c -> case toCardOwner c of
          Nothing -> pure False
          Just iid -> member iid <$> select whoMatcher
        _ -> pure False
     in
      checkSource s
  Matcher.SourceIsType t -> pure $ case t of
    AssetType -> case s of
      AssetSource _ -> True
      _ -> False
    EventType -> case s of
      EventSource _ -> True
      _ -> False
    SkillType -> case s of
      SkillSource _ -> True
      _ -> False
    PlayerTreacheryType -> case s of
      TreacherySource _ -> True
      _ -> False
    PlayerEnemyType -> case s of
      EnemySource _ -> True
      _ -> False
    TreacheryType -> case s of
      TreacherySource _ -> True
      _ -> False
    EnemyType -> case s of
      EnemySource _ -> True
      _ -> False
    LocationType -> case s of
      LocationSource _ -> True
      _ -> False
    EncounterAssetType -> case s of
      AssetSource _ -> True
      _ -> False
    ActType -> case s of
      ActSource _ -> True
      _ -> False
    AgendaType -> case s of
      AgendaSource _ -> True
      _ -> False
    StoryType -> case s of
      StorySource _ -> True
      _ -> False
    InvestigatorType -> case s of
      InvestigatorSource _ -> True
      _ -> False
    ScenarioType -> case s of
      ScenarioSource -> True
      _ -> False
  Matcher.EncounterCardSource -> pure $ case s of
    ActSource _ -> True
    AgendaSource _ -> True
    EnemySource _ -> True
    LocationSource _ -> True
    TreacherySource _ -> True
    _ -> False
