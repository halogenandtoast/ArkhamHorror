module Arkham.Cost
  ( module Arkham.Cost
  ) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card
import Arkham.GameValue
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Token ( Token )
import Data.Text qualified as T

data CostStatus = UnpaidCost | PaidCost
  deriving stock Eq

totalActionCost :: Cost -> Int
totalActionCost (ActionCost n) = n
totalActionCost (Costs xs) = sum $ map totalActionCost xs
totalActionCost _ = 0

totalResourceCost :: Cost -> Int
totalResourceCost (ResourceCost n) = n
totalResourceCost (Costs xs) = sum $ map totalResourceCost xs
totalResourceCost _ = 0

totalResourcePayment :: Payment -> Int
totalResourcePayment (ResourcePayment n) = n
totalResourcePayment (Payments xs) = sum $ map totalResourcePayment xs
totalResourcePayment _ = 0

totalClueCost :: Cost -> Int
totalClueCost (ClueCost n) = n
totalClueCost (Costs xs) = sum $ map totalClueCost xs
totalClueCost _ = 0

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

data Payment
  = ActionPayment Int
  | AdditionalActionPayment
  | CluePayment Int
  | DoomPayment Int
  | ResourcePayment Int
  | CardPayment Card
  | DiscardPayment [Target]
  | DiscardCardPayment [Card]
  | ExhaustPayment [Target]
  | RemovePayment [Target]
  | ExilePayment [Target]
  | UsesPayment Int
  | HorrorPayment Int
  | DamagePayment Int
  | DirectDamagePayment Int
  | InvestigatorDamagePayment Int
  | SkillIconPayment [SkillType]
  | Payments [Payment]
  | SealTokenPayment Token
  | NoPayment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cost
  = ActionCost Int
  | AdditionalActionsCost
  | ClueCost Int
  | GroupClueCost GameValue LocationMatcher
  | PlaceClueOnLocationCost Int
  | ExhaustCost Target
  | ExhaustAssetCost AssetMatcher
  | RemoveCost Target
  | Costs [Cost]
  | OrCost [Cost]
  | DamageCost Source Target Int
  | DirectDamageCost Source InvestigatorMatcher Int
  | InvestigatorDamageCost Source InvestigatorMatcher DamageStrategy Int
  | DiscardTopOfDeckCost Int
  | DiscardCost Target
  | DiscardCardCost Card
  | DiscardFromCost Int CostZone CardMatcher
  | DiscardDrawnCardCost
  | DiscardHandCost
  | DoomCost Source Target Int
  | EnemyDoomCost Int EnemyMatcher
  | ExileCost Target
  | HandDiscardCost Int CardMatcher
  | SkillIconCost Int (HashSet SkillType)
  | DiscardCombinedCost Int
  | ShuffleDiscardCost Int CardMatcher
  | HorrorCost Source Target Int
  | Free
  | ResourceCost Int
  | UseCost AssetMatcher UseType Int
  | UpTo Int Cost
  | SealCost TokenMatcher
  | SealTokenCost Token -- internal to track sealed token
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

displayCostType :: Cost -> Text
displayCostType = \case
  ActionCost n -> pluralize n "Action"
  DiscardTopOfDeckCost n -> pluralize n "Card" <> " from the top of your deck"
  DiscardCombinedCost n -> "Discard cards with a total combined cost of at least " <> tshow n
  DiscardHandCost -> "Discard your entire hand"
  ShuffleDiscardCost n _ -> "Shuffle " <> pluralize n "matching card" <> " into your deck"
  AdditionalActionsCost -> "Additional Action"
  ClueCost n -> pluralize n "Clue"
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
  PlaceClueOnLocationCost n ->
    "Place " <> pluralize n "Clue" <> " on your location"
  ExhaustCost _ -> "Exhaust"
  ExhaustAssetCost _ -> "Exhaust matching asset"
  RemoveCost _ -> "Remove from play"
  Costs cs -> T.intercalate ", " $ map displayCostType cs
  OrCost cs -> T.intercalate " or " $ map displayCostType cs
  DamageCost _ _ n -> tshow n <> " Damage"
  DirectDamageCost _ _ n -> tshow n <> " Direct Damage"
  InvestigatorDamageCost _ _ _ n -> tshow n <> " Damage"
  DiscardCost _ -> "Discard"
  DiscardCardCost _ -> "Discard Card"
  DiscardFromCost n _ _ -> "Discard " <> tshow n
  DiscardDrawnCardCost -> "Discard Drawn Card"
  DoomCost _ _ n -> pluralize n "Doom"
  EnemyDoomCost n _ -> "Place " <> pluralize n "Doom" <> " on a matching enemy"
  ExileCost _ -> "Exile"
  HandDiscardCost n _ -> "Discard " <> tshow n <> " from Hand"
  SkillIconCost n _ -> tshow n <> " Matching Icons"
  HorrorCost _ _ n -> tshow n <> " Horror"
  Free -> "Free"
  ResourceCost n -> pluralize n "Resource"
  UseCost _ uType n -> case uType of
    Ammo -> tshow n <> " Ammo"
    Supply -> if n == 1 then "1 Supply" else tshow n <> " Supplies"
    Secret -> pluralize n "Secret"
    Charge -> pluralize n "Charge"
    Try -> if n == 1 then "1 Try" else tshow n <> " Tries"
    Bounty -> if n == 1 then "1 Bounty" else tshow n <> " Bounties"
    Whistle -> pluralize n "Whistle"
    Resource -> pluralize n "Resource from the asset"
    Key -> pluralize n "Key"
  UpTo n c -> displayCostType c <> " up to " <> pluralize n "time"
  SealCost _ -> "Seal token"
  SealTokenCost _ -> "Seal token"
 where
  pluralize n a = if n == 1 then "1 " <> a else tshow n <> " " <> a <> "s"

instance Semigroup Cost where
  Free <> a = a
  a <> Free = a
  Costs xs <> Costs ys = Costs (xs <> ys)
  Costs xs <> a = Costs (a : xs)
  a <> Costs xs = Costs (a : xs)
  a <> b = Costs [a, b]

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CostZone where
  CostZones xs <> CostZones ys = CostZones (xs <> ys)
  CostZones xs <> y = CostZones (xs <> [y])
  x <> CostZones ys = CostZones (x : ys)
  x <> y = CostZones [x, y]
