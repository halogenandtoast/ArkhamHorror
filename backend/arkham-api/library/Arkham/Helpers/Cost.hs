module Arkham.Helpers.Cost where

import Arkham.Action (Action)
import Arkham.Asset.Cards.TheCircleUndone qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.Cost.FieldCost
import Arkham.Enemy.Types (Field (EnemySealedChaosTokens))
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Helpers.Calculation
import Arkham.Helpers.Card (extendedCardMatch)
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Customization
import Arkham.Helpers.GameValue
import Arkham.Helpers.Investigator (additionalActionCovers)
import Arkham.Helpers.Matchers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Ref
import Arkham.Helpers.Scenario
import {-# SOURCE #-} Arkham.Helpers.SkillTest
import Arkham.Helpers.SkillTest.Target
import Arkham.Helpers.Target
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
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
import Control.Monad.State.Strict (evalStateT, get, put)
import Data.List qualified as List

getCanAffordCost
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> [Action]
  -> [Window]
  -> Cost
  -> m Bool
getCanAffordCost iid source actions windows' =
  getCanAffordCost_ iid source actions windows' True

getCanAffordCost_
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> [Action]
  -> [Window]
  -> Bool
  -> Cost
  -> m Bool
getCanAffordCost_ !iid !(toSource -> source) !actions !windows' !canModify = \case
  ShuffleTopOfScenarioDeckIntoYourDeck n deckKey -> (>= n) . length <$> getScenarioDeck deckKey
  RemoveEnemyDamageCost x matcher -> do
    n <- getGameValue x
    selectAny $ matcher <> Matcher.EnemyWithDamage (Matcher.atLeast n)
  SpendKeyCost k -> fieldMap InvestigatorKeys (elem k) iid
  PlaceKeyCost _ k -> fieldMap InvestigatorKeys (elem k) iid
  GroupSpendKeyCost k lm -> selectAny (Matcher.InvestigatorAt lm <> Matcher.InvestigatorWithKey k)
  CostToEnterUnrevealed c -> getCanAffordCost_ iid source actions windows' canModify c
  UnpayableCost -> pure False
  ChooseEnemyCost mtcr -> selectAny mtcr
  ChooseExtendedCardCost mtcr -> selectAny mtcr
  ChosenEnemyCost eid -> selectAny (Matcher.EnemyWithId eid)
  ChosenCardCost cid -> selectAny (Matcher.basic $ Matcher.CardWithId cid)
  Free -> pure True
  UpTo {} -> pure True
  OptionalCost {} -> pure True
  AddCurseTokensEqualToShroudCost -> do
    mloc <- field InvestigatorLocation iid
    mShroud <- maybe (pure Nothing) (field LocationShroud) mloc
    case mShroud of
      Nothing -> pure False
      Just shroud -> do
        x <- getRemainingCurseTokens
        pure $ x >= shroud
  AddCurseTokensEqualToSkillTestDifficulty -> do
    getSkillTestDifficulty >>= \case
      Nothing -> pure False
      Just difficulty -> do
        x <- getRemainingCurseTokens
        pure $ x >= difficulty
  AddFrostTokenCost n -> do
    x <- getRemainingFrostTokens
    pure $ x >= n
  AddCurseTokenCost n -> do
    x <- getRemainingCurseTokens
    -- Are you Parallel Rex?
    canParallelRex <-
      iid
        <=~> ( Matcher.InvestigatorIs "90078"
                <> Matcher.InvestigatorAt Matcher.Anywhere
                <> Matcher.InvestigatorWithAnyClues
             )
    z <-
      if canParallelRex
        then fieldMap InvestigatorClues (* 2) iid
        else pure 0
    pure $ (x + z >= n)
  AddCurseTokensCost n _ -> do
    x <- getRemainingCurseTokens
    canParallelRex <-
      iid
        <=~> ( Matcher.InvestigatorIs "90078"
                <> Matcher.InvestigatorAt Matcher.Anywhere
                <> Matcher.InvestigatorWithAnyClues
             )
    z <-
      if canParallelRex
        then fieldMap InvestigatorClues (* 2) iid
        else pure 0
    pure $ x + z >= n
  SkillTestCost {} -> pure True
  AsIfAtLocationCost lid c -> do
    withModifiers' iid (toModifiers source [AsIfAt lid])
      $ getCanAffordCost_ iid source actions windows' canModify c
  ShuffleAttachedCardIntoDeckCost target cardMatcher -> do
    case target of
      AssetTarget aid -> fieldMap AssetCardsUnderneath (any (`cardMatch` cardMatcher)) aid
      _ -> error "Unhandled shuffle attached card into deck cost"
  EnemyAttackCost eid -> selectAny $ Matcher.EnemyWithId eid <> Matcher.EnemyCanAttack (Matcher.InvestigatorWithId iid)
  DrawEncounterCardsCost _n -> can.target.encounterDeck iid
  CostWhenEnemy mtchr c -> do
    hasEnemy <- selectAny mtchr
    if hasEnemy then getCanAffordCost_ iid source actions windows' canModify c else pure True
  CostIfEnemy mtchr c1 c2 -> do
    hasEnemy <- selectAny mtchr
    getCanAffordCost_ iid source actions windows' canModify $ if hasEnemy then c1 else c2
  CostIfCustomization customization c1 c2 -> do
    case source of
      (CardIdSource cid) -> do
        card <- getCard cid
        case card of
          PlayerCard pc ->
            getCanAffordCost_ iid source actions windows' canModify
              $ if pc `hasCustomization` customization then c1 else c2
          _ -> error "Not implemented"
      _ -> error "Not implemented"
  ArchiveOfConduitsUnidentifiedCost -> do
    n <- selectCount Matcher.Anywhere
    pure $ n >= 4
  NonBlankedCost c -> do
    mods <- getModifiers (sourceToTarget source)
    if Blank `elem` mods
      then pure True
      else getCanAffordCost_ iid source actions windows' canModify c
  GloriaCost -> do
    mtarget <- getSkillTestTarget
    case mtarget of
      Nothing -> pure False
      Just t -> do
        gloria <- selectJust $ Matcher.investigatorIs Investigators.gloriaGoldberg
        cardsUnderneath <- field InvestigatorCardsUnderneath gloria
        traits <- targetTraits t
        pure $ any (\trait -> any (`cardMatch` Matcher.CardWithTrait trait) cardsUnderneath) traits
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
  AdditionalActionsCostThatReducesResourceCostBy n cost -> do
    spendableActions <- field InvestigatorRemainingActions iid
    let totalActions = totalActionCost cost
    let reduction = max 0 ((spendableActions - totalActions) * n)
    withModifiers iid (toModifiers source [ExtraResources reduction]) do
      getCanAffordCost_ iid source actions windows' canModify cost
  RevealCost {} -> pure True
  Costs xs ->
    and <$> traverse (getCanAffordCost_ iid source actions windows' canModify) xs
  OrCost xs ->
    or <$> traverse (getCanAffordCost_ iid source actions windows' canModify) xs
  ExhaustCost target -> case target of
    AssetTarget aid ->
      elem aid <$> select Matcher.AssetReady
    EventTarget eid ->
      elem eid <$> select Matcher.EventReady
    _ -> error $ "Not handled" <> show target
  ExhaustAssetCost matcher ->
    selectAny $ matcher <> Matcher.AssetReady
  ExhaustXAssetCost matcher ->
    selectAny $ matcher <> Matcher.AssetReady
  DiscardAssetCost matcher ->
    selectAny $ matcher <> Matcher.DiscardableAsset
  UseCost assetMatcher uType n -> do
    assets <- select (Matcher.replaceYouMatcher iid assetMatcher)
    uses <- flip evalStateT assets $ do
      sum <$> for assets \asset -> do
        mods <- lift $ getModifiers asset
        alreadyCounted <- get
        fromOtherSources <-
          sum <$> for mods \case
            ProvidesUses uType' (AssetSource s) | uType' == uType -> do
              if s `elem` alreadyCounted
                then pure 0
                else do
                  put $ s : alreadyCounted
                  lift $ fieldMap AssetUses (findWithDefault 0 uType) s
            ProvidesProxyUses pType uType' (AssetSource s) | uType' == uType -> do
              if s `elem` alreadyCounted
                then pure 0
                else do
                  put $ s : alreadyCounted
                  lift $ fieldMap AssetUses (findWithDefault 0 pType) s
            _ -> pure 0

        lift $ fieldMap AssetUses ((+ fromOtherSources) . findWithDefault 0 uType) asset
    pure $ uses >= n
  EventUseCost eventMatcher uType n -> do
    events <- select eventMatcher
    uses <- sum <$> traverse (fieldMap EventUses (findWithDefault 0 uType)) events
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
    if any (`elem` modifiers) [ActionsAreFree, IgnoreActionCost]
      then pure True
      else do
        takenActions <- field InvestigatorActionsTaken iid
        performedActions <- field InvestigatorActionsPerformed iid
        let modifiedActionCost =
              if canModify
                then foldr (applyActionCostModifier takenActions performedActions actions) n modifiers
                else n
        additionalActions <- field InvestigatorAdditionalActions iid
        additionalActionCount <- countM (additionalActionCovers source actions) additionalActions
        actionCount <- field InvestigatorRemainingActions iid
        pure $ actionCount + additionalActionCount >= modifiedActionCost
  AdditionalActionCost -> getCanAffordCost_ iid source actions windows' canModify (ActionCost 1)
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
  GroupClueCostX -> do
    spendableClues <- getSpendableClueCount =<< select Matcher.UneliminatedInvestigator
    n <- perPlayer 1
    pure $ spendableClues >= n
  PlaceClueOnLocationCost n -> do
    canParallelRex <- iid <=~> Matcher.InvestigatorIs "90078"
    z <-
      if canParallelRex
        then (`div` 2) <$> getRemainingCurseTokens
        else pure 0
    spendableClues <- getSpendableClueCount [iid]
    pure $ (spendableClues + z) >= n
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
        FromHandOf whoMatcher -> do
          excludeCards <- case source of
            CardIdSource cid -> (: []) <$> getCard cid
            _ -> pure mempty
          fmap
            ( filterBy
                [ (`cardMatch` (cardMatcher <> Matcher.DiscardableCard))
                , (`notElem` excludeCards)
                ]
                . concat
            )
            . traverse (field InvestigatorHand)
            =<< select whoMatcher
        FromPlayAreaOf whoMatcher -> do
          assets <- select $ Matcher.AssetControlledBy whoMatcher <> Matcher.DiscardableAsset
          filterCards cardMatcher <$> traverse (field AssetCard) assets
        CostZones zs -> concatMapM getCards zs
    (>= n) . length <$> getCards zone
  DiscardUnderneathCardCost assetId cardMatcher -> do
    cards <- field AssetCardsUnderneath assetId
    anyM (<=~> cardMatcher) cards
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
  DoomCost _ (AgendaMatcherTarget agendaMatcher) _ -> selectAny agendaMatcher
  DoomCost {} -> pure True -- TODO: Make better
  EnemyDoomCost _ enemyMatcher -> selectAny enemyMatcher
  SkillIconCost n skillTypes -> do
    handCards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    let countF = if null skillTypes then const True else (`member` insertSet WildIcon skillTypes)
    let total = sum $ map (count countF . cdSkills . toCardDef) handCards
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
  HandDiscardCost n extendedCardMatcher -> do
    excludeCards <- case source of
      CardIdSource cid -> (: []) <$> getCard cid
      _ -> pure mempty
    cards <-
      mapMaybe (preview _PlayerCard) . filter (`notElem` excludeCards) <$> field InvestigatorHand iid
    (>= n)
      <$> countM (`extendedCardMatch` (Matcher.basic Matcher.DiscardableCard <> extendedCardMatcher)) cards
  HandDiscardAnyNumberCost extendedCardMatcher -> do
    excludeCards <- case source of
      CardIdSource cid -> (: []) <$> getCard cid
      _ -> pure mempty
    cards <-
      mapMaybe (preview _PlayerCard) . filter (`notElem` excludeCards) <$> field InvestigatorHand iid
    (> 0) <$> countM (`extendedCardMatch` extendedCardMatcher) cards
  ReturnMatchingAssetToHandCost assetMatcher -> selectAny $ Matcher.AssetCanLeavePlayByNormalMeans <> assetMatcher
  ReturnAssetToHandCost assetId -> selectAny $ Matcher.AssetWithId assetId
  SealCost tokenMatcher -> do
    tokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
    anyM (\token -> matchChaosToken iid token tokenMatcher) tokens
  SealMultiCost n tokenMatcher -> do
    tokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
    (>= n) <$> countM (\token -> matchChaosToken iid token tokenMatcher) tokens
  SealChaosTokenCost _ -> pure True
  ReleaseChaosTokensCost n tokenMatcher -> do
    case tokenMatcher of
      Matcher.SealedOnAsset assetMatcher tokenMatcher' -> do
        mAsset <- selectOne assetMatcher
        case mAsset of
          Nothing -> pure False
          Just aid ->
            fmap (>= n) . countM (<=~> Matcher.IncludeSealed tokenMatcher') =<< field AssetSealedChaosTokens aid
      Matcher.SealedOnEnemy enemyMatcher tokenMatcher' -> do
        mEnemy <- selectOne enemyMatcher
        case mEnemy of
          Nothing -> pure False
          Just eid ->
            fmap (>= n) . countM (<=~> Matcher.IncludeSealed tokenMatcher') =<< field EnemySealedChaosTokens eid
      _ -> do
        let
          handleSource = \case
            AssetSource aid ->
              fmap (>= n) . countM (<=~> Matcher.IncludeSealed tokenMatcher) =<< field AssetSealedChaosTokens aid
            AbilitySource t _ -> handleSource t
            _ -> error $ "Unhandled release token cost source: " <> show source
        handleSource source
  ReleaseChaosTokenCost t ->
    t
      <=~> Matcher.IncludeSealed
        ( Matcher.oneOf
            [ Matcher.SealedOnAsset Matcher.AnyAsset Matcher.AnyChaosToken
            , Matcher.SealedOnEnemy Matcher.AnyInPlayEnemy Matcher.AnyChaosToken
            ]
        )
  ReturnChaosTokensToPoolCost n matcher -> do
    (>= n) <$> selectCount matcher
  ReturnChaosTokenToPoolCost _ -> pure True
  FieldResourceCost (FieldCost mtchr fld) -> do
    ns <- selectFields fld mtchr
    resources <- getSpendableResources iid
    pure $ any (resources >=) ns
  MaybeFieldResourceCost (MaybeFieldCost mtchr fld) -> do
    ns <- catMaybes <$> selectFields fld mtchr
    resources <- getSpendableResources iid
    pure $ any (resources >=) ns
  CalculatedResourceCost calc -> do
    n <- calculate (Matcher.replaceYouMatcher iid calc)
    resources <- getSpendableResources iid
    pure $ resources >= n
  SupplyCost locationMatcher supply ->
    iid
      <=~> ( Matcher.InvestigatorWithSupply supply
              <> Matcher.InvestigatorAt locationMatcher
           )
  ResolveEachHauntedAbility _ -> pure True

getSpendableResources :: HasGame m => InvestigatorId -> m Int
getSpendableResources iid = do
  mods <- getModifiers iid
  let extraResources = sum [x | ExtraResources x <- mods]
  familyInheritanceResources <-
    getSum
      <$> selectAgg
        Sum
        AssetResources
        (Matcher.assetIs Assets.familyInheritance)
  fieldMap InvestigatorResources (+ (familyInheritanceResources + extraResources)) iid

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
    Just (MaxDynamicCost _) -> pure 0
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
applyActionCostModifier _ performedActions actions (AdditionalActionCostOf (FirstOneOfPerformed as) m) n
  | notNull (actions `List.intersect` as) && all (\a -> all (notElem a) performedActions) as =
      n + m
applyActionCostModifier _ _ _ (ActionCostModifier m) n = n + m
applyActionCostModifier _ _ _ _ n = n
