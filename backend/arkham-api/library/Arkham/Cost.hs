{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost (
  module Arkham.Cost,
  module X,
) where

import Arkham.Classes.GameLogger
import Arkham.Cost.Status as X
import Arkham.Zone as X

import Arkham.Asset.Uses
import Arkham.Calculation
import Arkham.Campaigns.TheForgottenAge.Supply
import {-# SOURCE #-} Arkham.Card
import Arkham.ChaosToken.Types (ChaosToken, ChaosTokenFace)
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.Cost.FieldCost
import Arkham.Customization
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import Arkham.Key
import Arkham.Matcher
import Arkham.Name
import Arkham.Plural
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Control.Lens (Plated (..), Prism', cosmos, prism', sumOf, toListOf, _2)
import Data.Aeson.TH
import Data.Data.Lens (uniplate)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Records

decreaseActionCost :: Cost -> Int -> Cost
decreaseActionCost (ActionCost x) y = ActionCost $ max 0 (x - y)
decreaseActionCost (Costs (a : as)) y = case a of
  ActionCost x | x >= y -> Costs (ActionCost (x - y) : as)
  ActionCost x ->
    ActionCost (max 0 (x - y)) <> decreaseActionCost (Costs as) (y - x)
  _ -> a <> decreaseActionCost (Costs as) y
decreaseActionCost other _ = other

increaseActionCost :: Cost -> Int -> Cost
increaseActionCost (ActionCost x) y = ActionCost $ max 0 (x + y)
increaseActionCost (Costs (a : as)) y = case a of
  ActionCost x -> Costs (ActionCost (x + y) : as)
  _ -> a <> increaseActionCost (Costs as) y
increaseActionCost other _ = other

increaseResourceCost :: Cost -> Int -> Cost
increaseResourceCost (ResourceCost x) y = ResourceCost $ max 0 (x + y)
increaseResourceCost (Costs (a : as)) y = case a of
  ResourceCost x -> Costs (ResourceCost (x + y) : as)
  _ -> a <> increaseResourceCost (Costs as) y
increaseResourceCost other _ = other

decreaseResourceCost :: Cost -> Int -> Cost
decreaseResourceCost (ResourceCost x) y = ResourceCost $ max 0 (x - y)
decreaseResourceCost (Costs (a : as)) y = case a of
  ResourceCost x -> Costs (ResourceCost (max 0 $ x - y) : as)
  _ -> a <> decreaseResourceCost (Costs as) y
decreaseResourceCost other _ = other

data Payment
  = ActionPayment Int
  | AdditionalActionPayment
  | ChosenEnemyPayment EnemyId
  | ChosenCardPayment CardId
  | CluePayment InvestigatorId Int
  | DoomPayment Int
  | ResourcePayment Int
  | CardPayment Card
  | DiscardPayment [(Zone, Card)]
  | DiscardCardPayment [Card]
  | ExhaustPayment [Target]
  | RemovePayment [Target]
  | ExilePayment [Target]
  | UsesPayment Int
  | HorrorPayment Int
  | DamagePayment Int
  | DirectDamagePayment Int
  | DirectHorrorPayment Int
  | InvestigatorDamagePayment Int
  | SkillIconPayment [SkillIcon]
  | Payments [Payment]
  | SealChaosTokenPayment ChaosToken
  | ReleaseChaosTokenPayment ChaosToken
  | ReturnChaosTokenToPoolPayment ChaosToken
  | ReturnToHandPayment Card
  | NoPayment
  | SupplyPayment Supply
  | AddCurseTokenPayment Int
  | AddFrostTokenPayment Int
  deriving stock (Show, Eq, Ord, Data)

instance Plated Payment where
  plate = uniplate

data Cost
  = ActionCost Int
  | UnlessFastActionCost Int -- Eldritch Tongue
  | IncreaseCostOfThis CardId Int
  | AdditionalActionCost -- use the plural instead as this is internal
  | AdditionalActionsCost
  | AdditionalActionsCostThatReducesResourceCostBy Int Cost
  | CostToEnterUnrevealed Cost
  | AssetClueCost Text AssetMatcher GameValue
  | ClueCost GameValue
  | ClueCostX
  | GroupClueCostX LocationMatcher
  | DiscoveredCluesCost
  | GroupResourceCost GameValue LocationMatcher
  | GroupDiscardCost GameValue ExtendedCardMatcher LocationMatcher
  | GroupClueCost GameValue LocationMatcher
  | SameLocationGroupClueCost GameValue LocationMatcher
  | GroupClueCostRange (Int, Int) LocationMatcher
  | PlaceClueOnLocationCost Int
  | ExhaustCost Target
  | ShuffleTopOfScenarioDeckIntoYourDeck Int ScenarioDeckKey
  | ChooseEnemyCost EnemyMatcher
  | ChooseEnemyCostAndMaybeFieldClueCost EnemyMatcher (Field Enemy (Maybe Int))
  | ChooseEnemyCostAndMaybeGroupFieldClueCost LocationMatcher EnemyMatcher (Field Enemy (Maybe Int))
  | ChosenEnemyCost EnemyId
  | ChooseExtendedCardCost ExtendedCardMatcher
  | ChosenCardCost CardId
  | DiscardAssetCost AssetMatcher
  | ExhaustAssetCost AssetMatcher
  | ExhaustXAssetCost AssetMatcher
  | RemoveCost Target
  | RevealCost CardId
  | Costs [Cost]
  | OrCost [Cost]
  | PlaceKeyCost Target ArkhamKey
  | SpendKeyCost ArkhamKey
  | SpendTokenKeyCost Int ChaosTokenFace
  | GroupSpendKeyCost ArkhamKey LocationMatcher
  | DiscardTopOfDeckCost Int
  | DiscardTopOfDeckWithTargetCost Target Int
  | DiscardCost Zone Target
  | DiscardCardCost Card
  | DiscardUnderneathCardCost AssetId ExtendedCardMatcher
  | DiscardRandomCardCost
  | DiscardFromCost Int CostZone CardMatcher
  | DiscardDrawnCardCost
  | DiscardHandCost
  | DoomCost Source Target Int
  | EnemyDoomCost Int EnemyMatcher
  | EnemyAttackCost EnemyId
  | RemoveEnemyDamageCost GameValue EnemyMatcher
  | ExileCost Target
  | HandDiscardCost Int ExtendedCardMatcher
  | HandDiscardAnyNumberCost ExtendedCardMatcher
  | ReturnMatchingAssetToHandCost AssetMatcher
  | ReturnAssetToHandCost AssetId
  | ReturnEventToHandCost EventId
  | SkillTestCost Source SkillType GameCalculation
  | SkillIconCost Int (Set SkillIcon)
  | SkillIconCostMatching Int (Set SkillIcon) ExtendedCardMatcher
  | SameSkillIconCost Int
  | SameSkillIconCostMatching Int ExtendedCardMatcher
  | DiscardCombinedCost Int
  | ShuffleDiscardCost Int CardMatcher
  | Free
  | ScenarioResourceCost Int
  | ResourceCost Int
  | CalculatedResourceCost GameCalculation
  | CalculatedHandDiscardCost GameCalculation ExtendedCardMatcher
  | FieldResourceCost FieldCost
  | MaybeFieldResourceCost MaybeFieldCost
  | UseCost AssetMatcher UseType Int
  | AllUsesCost AssetMatcher UseType
  | EventUseCost EventMatcher UseType Int
  | DynamicUseCost AssetMatcher UseType DynamicUseCostValue
  | UseCostUpTo AssetMatcher UseType Int Int -- (e.g. Spend 1-5 ammo, see M1918 BAR)
  | CostWhenEnemy EnemyMatcher Cost
  | CostWhenTreachery TreacheryMatcher Cost
  | CostWhenTreacheryElse TreacheryMatcher Cost Cost
  | CostIfEnemy EnemyMatcher Cost Cost
  | CostIfCustomization Customization Cost Cost
  | CostIfRemembered ScenarioLogKey Cost Cost
  | UpTo GameCalculation Cost
  | AtLeastOne GameCalculation Cost
  | SealCost ChaosTokenMatcher
  | SealMultiCost Int ChaosTokenMatcher
  | AddFrostTokenCost Int
  | AddCurseTokenCost Int
  | AddCurseTokensCost Int Int
  | AddCurseTokensEqualToShroudCost
  | AddCurseTokensEqualToSkillTestDifficulty
  | ReleaseChaosTokenCost ChaosToken
  | ReleaseChaosTokensCost Int ChaosTokenMatcher
  | SealChaosTokenCost ChaosToken -- internal to track sealed token
  | ReturnChaosTokensToPoolCost Int ChaosTokenMatcher
  | ReturnChaosTokenToPoolCost ChaosToken
  | SupplyCost LocationMatcher Supply
  | ResolveEachHauntedAbility LocationId -- the circle undone, see TrappedSpirits
  | ShuffleBondedCost Int CardCode
  | ShuffleIntoDeckCost Target
  | ShuffleAttachedCardIntoDeckCost Target CardMatcher
  | OptionalCost Cost
  | UnpayableCost
  | AsIfAtLocationCost LocationId Cost
  | NonBlankedCost Cost
  | DrawEncounterCardsCost Int
  | GloriaCost -- lol, not going to attempt to make this generic
  | ArchiveOfConduitsUnidentifiedCost -- this either
  | LabeledCost Text Cost
  | FlipScarletKeyCost
  | -- We do the costs that can kill the investigator last so we don't trigger discards before the cost is paid
    DirectHorrorCost Source InvestigatorMatcher Int
  | DirectDamageAndHorrorCost Source InvestigatorMatcher Int Int
  | HorrorCost Source Target Int
  | HorrorCostX Source -- for The Black Book
  | DamageCost Source Target Int
  | DirectDamageCost Source InvestigatorMatcher Int
  | InvestigatorDamageCost Source InvestigatorMatcher DamageStrategy Int
  | ConcealedXCost
  | XCost Cost
  | OneOfDistanceCost LocationMatcher Cost
  deriving stock (Show, Eq, Ord, Data)

instance Plated Cost

clueCost :: Int -> Cost
clueCost n = ClueCost (Static n)

assetUseCost :: (Entity a, EntityId a ~ AssetId) => a -> UseType -> Int -> Cost
assetUseCost a uType n = UseCost (AssetWithId $ toId a) uType n

dynamicAssetUseCost :: (Entity a, EntityId a ~ AssetId) => a -> UseType -> GameCalculation -> Cost
dynamicAssetUseCost a uType c = DynamicUseCost (AssetWithId $ toId a) uType (DynamicCalculation c)

exhaust :: Targetable a => a -> Cost
exhaust = ExhaustCost . toTarget

damageCost :: (Sourceable source, Targetable source) => source -> Int -> Cost
damageCost source = DamageCost (toSource source) (toTarget source)

horrorCost :: (Sourceable source, Targetable source) => source -> Int -> Cost
horrorCost source = HorrorCost (toSource source) (toTarget source)

discardCost :: Targetable a => a -> Cost
discardCost = DiscardCost FromPlay . toTarget

exileCost :: Targetable a => a -> Cost
exileCost = ExileCost . toTarget

removeCost :: Targetable a => a -> Cost
removeCost = RemoveCost . toTarget

data DynamicUseCostValue = DrawnCardsValue | DynamicCalculation GameCalculation
  deriving stock (Show, Eq, Ord, Data)

displayCostType :: Cost -> Text
displayCostType = \case
  ConcealedXCost -> "Concealed X"
  XCost c -> "X " <> displayCostType c
  OneOfDistanceCost _ c -> "X " <> displayCostType c
  LabeledCost lbl _ -> lbl
  FlipScarletKeyCost -> "Flip a Stable key you control to its Unstable side"
  ShuffleTopOfScenarioDeckIntoYourDeck n deckKey -> "Shuffle top " <> tshow n <> " cards of the " <> toDisplay deckKey <> " deck into your deck"
  RemoveEnemyDamageCost _n _k -> "Remove damage from enemy"
  SpendKeyCost k -> "Spend " <> keyName k <> " Key"
  SpendTokenKeyCost n tkn -> "Spend " <> tshow n <> " " <> tshow tkn <> " " <> pluralize n "Key"
  PlaceKeyCost _ k -> "Place " <> keyName k <> " Key"
  GroupSpendKeyCost k _ -> "Spend " <> keyName k <> " Key"
  CostToEnterUnrevealed c -> "As an additional cost for you to enter, pay " <> displayCostType c
  GroupClueCostX _ -> "X {perPlayer} clues as a group"
  DiscoveredCluesCost -> "Spend discovered clues"
  ChooseEnemyCost _ -> "Choose an enemy"
  ChooseEnemyCostAndMaybeFieldClueCost _ _ -> "Choose an enemy and spend X clues"
  ChooseEnemyCostAndMaybeGroupFieldClueCost {} -> "Investigators spend X clues"
  ChooseExtendedCardCost _ -> "Choose a card that matches"
  ChosenEnemyCost _ -> "Choose an enemy"
  ChosenCardCost _ -> "Choose a card that matches"
  ExhaustXAssetCost _ -> "Exhaust X copies"
  EnemyAttackCost _ -> "The chosen enemy makes an attack against you"
  UnpayableCost -> "Unpayable"
  OptionalCost c -> "Optional: " <> displayCostType c
  DrawEncounterCardsCost n -> "Draw " <> pluralize n "Encounter Card"
  NonBlankedCost c -> displayCostType c
  GloriaCost ->
    "Discard an encounter card that shares a Trait with that encounter card from beneath Gloria Goldberg"
  ArchiveOfConduitsUnidentifiedCost ->
    "Place 1 resource on 4 different locations, as leylines."
  CostWhenEnemy _ c -> displayCostType c
  CostWhenTreachery _ c -> displayCostType c
  CostWhenTreacheryElse _ _ c -> displayCostType c
  CostIfEnemy _ _ c -> displayCostType c
  CostIfCustomization _ _ c -> displayCostType c
  CostIfRemembered _ _ c -> displayCostType c
  AsIfAtLocationCost _ c -> displayCostType c
  ShuffleAttachedCardIntoDeckCost _ _ -> "Shuffle attached card into deck"
  AddFrostTokenCost n -> "Add " <> tshow n <> " {frost} " <> pluralize n "token" <> "to the chaos bag"
  AddCurseTokenCost n -> "Add " <> tshow n <> " {curse} " <> pluralize n "token" <> "to the chaos bag"
  AddCurseTokensCost n m -> "Add " <> tshow n <> "-" <> tshow m <> " {curse} tokens to the chaos bag"
  AddCurseTokensEqualToShroudCost -> "Add {curse} tokens to the chaos bag equal to your location's shroud value"
  AddCurseTokensEqualToSkillTestDifficulty -> "Add {curse} tokens to this test's difficulty"
  ShuffleIntoDeckCost _ -> "Shuffle into deck"
  ShuffleBondedCost n cCode -> case lookupCardDef cCode of
    Just def ->
      "You must search your bonded cards for "
        <> pluralize n "copy"
        <> " of "
        <> toTitle def
        <> " into your deck"
    Nothing -> error "impossible"
  ResolveEachHauntedAbility _ -> "Resolve each haunted ability on this location"
  ActionCost n -> pluralize n "Action"
  UnlessFastActionCost n -> pluralize n "Action"
  DiscardTopOfDeckCost n -> pluralize n "Card" <> " from the top of your deck"
  DiscardTopOfDeckWithTargetCost _ n -> pluralize n "Card" <> " from the top of your deck"
  DiscardAssetCost _ -> "Discard matching asset"
  DiscardCombinedCost n ->
    "Discard cards with a total combined cost of at least " <> tshow n
  DiscardHandCost -> "Discard your entire hand"
  ShuffleDiscardCost n _ ->
    "Shuffle " <> pluralize n "matching card" <> " into your deck"
  AdditionalActionCost -> "Additional Action"
  AdditionalActionsCost -> "Additional Action"
  AdditionalActionsCostThatReducesResourceCostBy _ _ -> "Additional Action"
  AssetClueCost lbl _ gv -> case gv of
    Static n -> pluralize n "Clue" <> " from " <> lbl
    PerPlayer n -> pluralize n "Clue" <> " per Player from " <> lbl
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Clues per Player from " <> lbl
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Clues for 1, 2, 3, or 4 players from "
        <> lbl
  ClueCost gv -> case gv of
    Static n -> pluralize n "Clue"
    PerPlayer n -> pluralize n "Clue" <> " per Player"
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Clues per Player"
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Clues for 1, 2, 3, or 4 players"
  ClueCostX -> "Spend X Clues"
  SameLocationGroupClueCost gv _ -> case gv of
    Static n -> pluralize n "Clue" <> " as a Group"
    PerPlayer n -> pluralize n "Clue" <> " per Player as a Group"
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Clues per Player"
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Clues for 1, 2, 3, or 4 players"
  GroupClueCost gv _ -> case gv of
    Static n -> pluralize n "Clue" <> " as a Group"
    PerPlayer n -> pluralize n "Clue" <> " per Player as a Group"
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Clues per Player"
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Clues for 1, 2, 3, or 4 players"
  GroupResourceCost gv _ -> case gv of
    Static n -> pluralize n "Resource" <> " as a Group"
    PerPlayer n -> pluralize n "Resource" <> " per Player as a Group"
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Resources per Player"
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Resources for 1, 2, 3, or 4 players"
  GroupDiscardCost gv _ _ -> case gv of
    Static n -> "Discard " <> pluralize n "Card" <> " as a Group"
    PerPlayer n -> "Discard " <> pluralize n "Card" <> " per Player as a Group"
    StaticWithPerPlayer n m ->
      "Discard " <> tshow n <> " + " <> tshow m <> " Cards per Player"
    ByPlayerCount a b c d ->
      "Discard "
        <> tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Cards for 1, 2, 3, or 4 players"
  GroupClueCostRange (sVal, eVal) _ ->
    tshow sVal <> "-" <> pluralize eVal "Clue" <> " as a Group"
  PlaceClueOnLocationCost n ->
    "Place " <> pluralize n "Clue" <> " on your location"
  ExhaustCost _ -> "Exhaust"
  ExhaustAssetCost _ -> "Exhaust matching asset"
  RemoveCost _ -> "Remove from play"
  RevealCost _ -> "Reveal this card"
  Costs cs -> T.intercalate ", " $ map displayCostType cs
  OrCost cs -> T.intercalate " or " $ map displayCostType cs
  DamageCost _ _ n -> tshow n <> " Damage"
  DirectDamageCost _ _ n -> tshow n <> " Direct Damage"
  DirectHorrorCost _ _ n -> tshow n <> " Direct Horror"
  DirectDamageAndHorrorCost _ _ m n -> tshow m <> " Direct Damage and " <> tshow n <> " Direct Horror"
  InvestigatorDamageCost _ _ _ n -> tshow n <> " Damage"
  DiscardCost zone _ -> "Discard from " <> zoneLabel zone
  DiscardCardCost _ -> "Discard Card"
  DiscardUnderneathCardCost _ _ -> "Discard card beneath"
  DiscardRandomCardCost -> "Discard Random Card"
  DiscardFromCost n _ _ -> "Discard " <> tshow n
  DiscardDrawnCardCost -> "Discard Drawn Card"
  DoomCost _ _ n -> pluralize n "Doom"
  EnemyDoomCost n _ -> "Place " <> pluralize n "Doom" <> " on a matching enemy"
  ExileCost _ -> "Exile"
  HandDiscardCost n _ -> "Discard " <> tshow n <> " from Hand"
  HandDiscardAnyNumberCost _ -> "Discard any number of cards from you hand"
  ReturnMatchingAssetToHandCost {} -> "Return matching asset to hand"
  ReturnAssetToHandCost {} -> "Return asset to hand"
  ReturnEventToHandCost {} -> "Return event to hand"
  SkillIconCost n _ -> tshow n <> " Matching Icons"
  SkillIconCostMatching n _ _ -> tshow n <> " Matching Icons"
  SkillTestCost _ sType n -> "Test {" <> tshow sType <> "}(" <> tshow n <> ")"
  SameSkillIconCost n -> tshow n <> " instances of the same skill icon"
  SameSkillIconCostMatching n _ -> tshow n <> " instances of the same skill icon"
  HorrorCost _ _ n -> tshow n <> " Horror"
  HorrorCostX _ -> "Take X Horror"
  Free -> "Free"
  ResourceCost n -> pluralize n "Resource"
  ScenarioResourceCost n -> pluralize n "Resource from the scenario reference"
  EventUseCost _ b c -> displayCostType (UseCost AnyAsset b c)
  AllUsesCost _ uType -> case uType of
    Offering -> "All Offerings"
    _ -> "All Uses"
  UseCost _ uType n -> pluralize n (T.pack $ splitCamelCase $ show uType)
  DynamicUseCost _ uType _ -> "X " <> pluralize_ 0 (T.pack $ splitCamelCase $ show uType)
  UseCostUpTo _ uType n m -> tshow n <> "-" <> tshow m <> " " <> pluralize_ 0 (T.pack $ splitCamelCase $ show uType)
  UpTo (Fixed n) c -> displayCostType c <> " up to " <> pluralize n "time"
  UpTo _ c -> displayCostType c <> " up to X times"
  AtLeastOne (Fixed n) c -> displayCostType c <> " up to " <> pluralize n "time"
  AtLeastOne _ c -> displayCostType c <> " up to X times"
  SealCost chaosTokenMatcher -> "Seal " <> toDisplay chaosTokenMatcher
  SealMultiCost n _ -> "Seal " <> tshow n <> " matching tokens"
  SealChaosTokenCost _ -> "Seal token"
  ReleaseChaosTokenCost _ -> "Release a chaos token sealed here"
  ReleaseChaosTokensCost 1 _ -> "Release a chaos token sealed here"
  ReleaseChaosTokensCost _ _ -> "Release chaos tokens sealed here"
  ReturnChaosTokensToPoolCost n (IncludeSealed _) ->
    "Search the chaos bag and/or cards in play for a total of "
      <> tshow n
      <> " matching tokens and return them to the token pool"
  ReturnChaosTokensToPoolCost n _ ->
    "Search the chaos bag for a total of "
      <> tshow n
      <> " matching tokens and return them to the token pool"
  ReturnChaosTokenToPoolCost token ->
    "Return " <> format token <> " to the token pool"
  FieldResourceCost {} -> "X"
  MaybeFieldResourceCost {} -> "X"
  CalculatedResourceCost {} -> "X"
  CalculatedHandDiscardCost {} -> "X"
  SupplyCost _ supply ->
    "An investigator crosses off " <> tshow supply <> " from their supplies"
  IncreaseCostOfThis _ n -> "Increase its cost by " <> tshow n

instance Semigroup Cost where
  OrCost cs <> OrCost ys = OrCost [c <> y | c <- cs, y <- ys]
  OrCost cs <> b = OrCost [c <> b | c <- cs]
  a <> OrCost cs = OrCost [a <> c | c <- cs]
  AdditionalActionsCostThatReducesResourceCostBy n1 a <> AdditionalActionsCostThatReducesResourceCostBy n2 b = AdditionalActionsCostThatReducesResourceCostBy (max n1 n2) (a <> b)
  AdditionalActionsCostThatReducesResourceCostBy n a <> b = AdditionalActionsCostThatReducesResourceCostBy n (a <> b)
  a <> AdditionalActionsCostThatReducesResourceCostBy n b = AdditionalActionsCostThatReducesResourceCostBy n (a <> b)
  Free <> a = a
  a <> Free = a
  ActionCost x <> ActionCost y = ActionCost (x + y)
  ResourceCost x <> ResourceCost y = ResourceCost (x + y)
  Costs xs <> Costs ys = Costs (combineCosts $ sort $ xs <> ys)
  Costs xs <> a = Costs (combineCosts $ sort $ a : xs)
  a <> Costs xs = Costs (combineCosts $ sort $ a : xs)
  a <> b = Costs $ sort [a, b]

combineCosts :: [Cost] -> [Cost]
combineCosts l@[] = l
combineCosts l@[_] = l
combineCosts (x : y : rest) = case (x, y) of
  (ClueCost (Static a), ClueCost (Static b)) -> combineCosts $ ClueCost (Static $ a + b) : rest
  (ClueCost (PerPlayer a), ClueCost (PerPlayer b)) -> combineCosts $ ClueCost (PerPlayer $ a + b) : rest
  (ClueCost (Static a), ClueCost (PerPlayer b)) -> combineCosts $ ClueCost (StaticWithPerPlayer a b) : rest
  (ClueCost (PerPlayer a), ClueCost (Static b)) -> combineCosts $ ClueCost (StaticWithPerPlayer b a) : rest
  (ActionCost a, ActionCost b) -> combineCosts $ ActionCost (a + b) : rest
  (ResourceCost a, ResourceCost b) -> combineCosts $ ResourceCost (a + b) : rest
  (HorrorCost s1 t1 a, HorrorCost s2 t2 b) | s1 == s2 && t1 == t2 -> combineCosts $ HorrorCost s1 t1 (a + b) : rest
  _ -> x : combineCosts (y : rest)

instance Monoid Cost where
  mempty = Free

instance Semigroup Payment where
  NoPayment <> a = a
  a <> NoPayment = a
  Payments xs <> Payments ys = Payments (xs <> ys)
  Payments xs <> a = Payments (a : xs)
  a <> Payments xs = Payments (a : xs)
  a <> b = Payments [a, b]

data CostZone
  = FromHandOf InvestigatorMatcher
  | FromPlayAreaOf InvestigatorMatcher
  | CostZones [CostZone]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup CostZone where
  CostZones xs <> CostZones ys = CostZones (xs <> ys)
  CostZones xs <> y = CostZones (xs <> [y])
  x <> CostZones ys = CostZones (x : ys)
  x <> y = CostZones [x, y]

mconcat
  [ deriveJSON defaultOptions ''CostZone
  , deriveJSON defaultOptions ''DynamicUseCostValue
  , deriveToJSON defaultOptions ''Cost
  , deriveJSON defaultOptions ''Payment
  , makePrisms ''Payment
  , makePrisms ''Cost
  ]

instance FromJSON Cost where
  parseJSON = withObject "Cost" \o -> do
    costType <- o .: "tag"
    case (costType :: Text) of
      "GroupClueCostX" -> do
        mcontents <- o .:? "contents"
        pure $ GroupClueCostX $ fromMaybe Anywhere mcontents
      _ -> $(mkParseJSON defaultOptions ''Cost) (Object o)

totalActionCost :: Cost -> Int
totalActionCost = sumOf (cosmos . _ActionCost)

totalResourcePayment :: Payment -> Int
totalResourcePayment = sumOf (cosmos . _ResourcePayment)

totalCluePayment :: Payment -> Int
totalCluePayment = sumOf (cosmos . _CluePayment . _2)

totalCluePaymentPerInvestigator :: Payment -> Map InvestigatorId Int
totalCluePaymentPerInvestigator = Map.fromListWith (+) . toListOf (cosmos . _CluePayment)

totalUsesPayment :: Payment -> Int
totalUsesPayment = sumOf (cosmos . _UsesPayment)

totalInvestigatorDamagePayment :: Payment -> Int
totalInvestigatorDamagePayment = sumOf (cosmos . _InvestigatorDamagePayment)

totalResourceCost :: Cost -> Int
totalResourceCost = sumOf (cosmos . _ResourceCost)

totalDiscardCardPayments :: Payment -> Int
totalDiscardCardPayments = length . concat . toListOf (cosmos . _DiscardCardPayment)

discardPayments :: Payment -> [(Zone, Card)]
discardPayments = concat . toListOf (cosmos . _DiscardPayment)

discardPayment :: Payment -> Maybe Card
discardPayment = preview (cosmos . _DiscardPayment . _singleton . _2)

_singleton :: Prism' [a] a
_singleton = prism' (: []) \case
  [a] -> Just a
  _ -> Nothing

chosenEnemyPayment :: Payment -> Maybe EnemyId
chosenEnemyPayment = listToMaybe . toListOf (cosmos . _ChosenEnemyPayment)

chosenCardPayment :: Payment -> Maybe CardId
chosenCardPayment = listToMaybe . toListOf (cosmos . _ChosenCardPayment)

addedCurseTokenPayment :: Payment -> Int
addedCurseTokenPayment = sum . toListOf (cosmos . _AddCurseTokenPayment)

discardedCards :: Payment -> [Card]
discardedCards = concat . toListOf (cosmos . _DiscardCardPayment)

exhaustedPayments :: Payment -> [Target]
exhaustedPayments = concat . toListOf (cosmos . _ExhaustPayment)

removedPayments :: Payment -> [Target]
removedPayments = concat . toListOf (cosmos . _RemovePayment)

horrorPaid :: Payment -> Int
horrorPaid = sumOf (cosmos . _HorrorPayment)

instance HasField "discards" Payment [(Zone, Card)] where
  getField = discardPayments

instance HasField "exhausted" Payment [Target] where
  getField = exhaustedPayments

instance HasField "resources" Payment Int where
  getField = totalResourcePayment

instance HasField "horror" Payment Int where
  getField = horrorPaid

instance HasField "resources" (Maybe Payment) Int where
  getField = maybe 0 totalResourcePayment

instance HasField "investigatorDamage" Payment Int where
  getField = totalInvestigatorDamagePayment
