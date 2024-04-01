module Arkham.Helpers.Cost where

import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.Cost.FieldCost
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.GameValue
import Arkham.Helpers.Investigator (additionalActionCovers)
import Arkham.Helpers.Matchers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Control.Lens (non)
import Data.List qualified as List

getCanAffordCost
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> [Action]
  -> [Window]
  -> Cost
  -> m Bool
getCanAffordCost iid (toSource -> source) actions windows' = \case
  UnpayableCost -> pure False
  Free -> pure True
  UpTo {} -> pure True
  OptionalCost {} -> pure True
  AddCurseTokenCost n -> do
    x <- getRemainingCurseTokens
    pure $ x >= n
  SkillTestCost {} -> pure True
  AsIfAtLocationCost lid c -> do
    withModifiers' iid (toModifiers source [AsIfAt lid])
      $ getCanAffordCost iid source actions windows' c
  ShuffleAttachedCardIntoDeckCost target cardMatcher -> do
    case target of
      AssetTarget aid -> fieldMap AssetCardsUnderneath (any (`cardMatch` cardMatcher)) aid
      _ -> error "Unhandled shuffle attached card into deck cost"
  ShuffleIntoDeckCost target -> case target of
    TreacheryTarget tid ->
      andM
        [ can.manipulate.deck iid
        , selectAny $ Matcher.TreacheryWithId tid
        ]
    AssetTarget tid -> andM [can.manipulate.deck iid, selectAny $ Matcher.AssetWithId tid]
    _ -> error "Unhandled shuffle into deck cost"
  ShuffleBondedCost n cardCode -> do
    bondedCards <- field InvestigatorBondedCards iid
    pure $ count ((== cardCode) . toCardCode) bondedCards >= n
  DiscardHandCost {} -> pure True
  DiscardTopOfDeckCost {} -> pure True
  AdditionalActionsCost {} -> pure True
  RevealCost {} -> pure True
  Costs xs ->
    and <$> traverse (getCanAffordCost iid source actions windows') xs
  OrCost xs ->
    or <$> traverse (getCanAffordCost iid source actions windows') xs
  ExhaustCost target -> case target of
    AssetTarget aid ->
      elem aid <$> select Matcher.AssetReady
    EventTarget eid ->
      elem eid <$> select Matcher.EventReady
    _ -> error $ "Not handled" <> show target
  ExhaustAssetCost matcher ->
    selectAny $ matcher <> Matcher.AssetReady
  DiscardAssetCost matcher ->
    selectAny $ matcher <> Matcher.DiscardableAsset
  UseCost assetMatcher uType n -> do
    assets <- select assetMatcher
    uses <-
      sum <$> traverse (fieldMap AssetUses (findWithDefault 0 uType)) assets
    pure $ uses >= n
  DynamicUseCost assetMatcher uType useCost -> case useCost of
    DrawnCardsValue -> do
      let
        toDrawnCards = \case
          (windowType -> Window.DrawCards _ xs) -> length xs
          _ -> 0
        drawnCardsValue = sum $ map toDrawnCards windows'
      assets <- select assetMatcher
      uses <-
        sum <$> traverse (fieldMap AssetUses (findWithDefault 0 uType)) assets
      pure $ uses >= drawnCardsValue
  UseCostUpTo assetMatcher uType n _ -> do
    assets <- select assetMatcher
    uses <-
      sum <$> traverse (fieldMap AssetUses (findWithDefault 0 uType)) assets
    pure $ uses >= n
  ActionCost n -> do
    modifiers <- getModifiers (InvestigatorTarget iid)
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        takenActions <- field InvestigatorActionsTaken iid
        performedActions <- field InvestigatorActionsPerformed iid
        let modifiedActionCost = foldr (applyActionCostModifier takenActions performedActions actions) n modifiers
        additionalActions <- field InvestigatorAdditionalActions iid
        additionalActionCount <- countM (additionalActionCovers source actions) additionalActions
        actionCount <- field InvestigatorRemainingActions iid
        pure $ (actionCount + additionalActionCount) >= modifiedActionCost
  AssetClueCost _ aMatcher gv -> do
    totalClueCost <- getPlayerCountValue gv
    clues <- getSum <$> selectAgg Sum AssetClues aMatcher
    pure $ clues >= totalClueCost
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
    iids <- select $ Matcher.InvestigatorAt locationMatcher
    totalSpendableClues <- getSpendableClueCount iids
    pure $ totalSpendableClues >= cost
  GroupClueCostRange (cost, _) locationMatcher -> do
    iids <- select $ Matcher.InvestigatorAt locationMatcher
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
            =<< select whoMatcher
        FromPlayAreaOf whoMatcher -> do
          assets <- select $ Matcher.AssetControlledBy whoMatcher
          traverse (field AssetCard) assets
        CostZones zs -> concatMapM getCards zs
    (> n) . length <$> getCards zone
  DiscardCost _ _ -> pure True -- TODO: Make better
  DiscardCardCost _ -> pure True -- TODO: Make better
  DiscardRandomCardCost -> iid <=~> Matcher.InvestigatorWithDiscardableCard
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
  SameSkillIconCost n -> do
    handCards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    let total = unionsWith (+) $ map (frequencies . cdSkills . toCardDef) handCards
    let wildCount = total ^. at #wild . non 0
    pure $ foldr (\x y -> y || x + wildCount >= n) False $ toList $ deleteMap #wild total
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
    let
      handleSource = \case
        AssetSource aid -> fieldMap AssetSealedChaosTokens ((>= n) . length) aid
        AbilitySource t _ -> handleSource t
        _ -> error $ "Unhandled release token cost source: " <> show source
    handleSource source
  ReleaseChaosTokenCost t -> do
    let
      handleSource = \case
        AssetSource aid -> fieldMap AssetSealedChaosTokens (elem t) aid
        AbilitySource u _ -> handleSource u
        _ -> error "Unhandled release token cost source"
    handleSource source
  FieldResourceCost (FieldCost mtchr fld) -> do
    ns <- selectFields fld mtchr
    resources <- getSpendableResources iid
    pure $ any (resources >=) ns
  MaybeFieldResourceCost (MaybeFieldCost mtchr fld) -> do
    ns <- catMaybes <$> selectFields fld mtchr
    resources <- getSpendableResources iid
    pure $ any (resources >=) ns
  SupplyCost locationMatcher supply ->
    iid
      <=~> ( Matcher.InvestigatorWithSupply supply
              <> Matcher.InvestigatorAt locationMatcher
           )
  ResolveEachHauntedAbility _ -> pure True

getSpendableResources :: HasGame m => InvestigatorId -> m Int
getSpendableResources iid = do
  familyInheritanceResources <-
    getSum
      <$> selectAgg
        Sum
        AssetResources
        (Matcher.assetIs Assets.familyInheritance)
  fieldMap InvestigatorResources (+ familyInheritanceResources) iid

getModifiedCardCost :: HasGame m => InvestigatorId -> Card -> m Int
getModifiedCardCost iid c@(PlayerCard _) = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  cardModifiers <- getModifiers (CardIdTarget $ toCardId c)
  startingCost <- getStartingCost
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  pcDef = toCardDef c
  getStartingCost = case cdCost pcDef of
    Just (StaticCost n) -> pure n
    Just DynamicCost -> pure 0
    Just DiscardAmountCost -> fieldMap InvestigatorDiscard (count ((== toCardCode c) . toCardCode)) iid
    Nothing -> pure 0
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

getSpendableClueCount :: HasGame m => [InvestigatorId] -> m Int
getSpendableClueCount investigatorIds =
  getSum
    <$> selectAgg
      Sum
      InvestigatorClues
      ( Matcher.InvestigatorWithoutModifier CannotSpendClues
          <> Matcher.AnyInvestigator (map Matcher.InvestigatorWithId investigatorIds)
      )

applyActionCostModifier
  :: [[Action]] -> [[Action]] -> [Action] -> ModifierType -> Int -> Int
applyActionCostModifier _ _ actions (ActionCostOf (IsAction action') m) n
  | action' `elem` actions = n + m
applyActionCostModifier _ performedActions actions (ActionCostOf (FirstOneOfPerformed as) m) n
  | notNull (actions `List.intersect` as) && all (\a -> all (notElem a) performedActions) as =
      n + m
applyActionCostModifier _ _ _ (ActionCostModifier m) n = n + m
applyActionCostModifier _ _ _ _ n = n
