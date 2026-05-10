{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost (module Arkham.Cost, module X) where

import Arkham.Cost.Status as X
import Arkham.Zone as X

import Arkham.Asset.Uses
import Arkham.Calculation
import Arkham.Campaigns.TheForgottenAge.Supply
import {-# SOURCE #-} Arkham.Card
import Arkham.ChaosToken.Types (ChaosToken, ChaosTokenFace)
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.Cost.FieldCost
import Arkham.Criteria
import Arkham.Customization
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import Arkham.Key
import Arkham.Matcher
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
  | SpendTokenCost Token TargetMatcher
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
  | CostOnlyWhen Criterion Cost
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
  | EachInvestigatorDamageCost Source InvestigatorMatcher DamageStrategy Int
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
