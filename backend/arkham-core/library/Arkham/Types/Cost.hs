module Arkham.Types.Cost
  ( module Arkham.Types.Cost
  ) where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

totalActionCost :: Cost -> Int
totalActionCost (ActionCost n) = n
totalActionCost (Costs xs) = sum $ map totalActionCost xs
totalActionCost _ = 0

totalResourceCost :: Cost -> Int
totalResourceCost (ResourceCost n) = n
totalResourceCost (Costs xs) = sum $ map totalResourceCost xs
totalResourceCost _ = 0

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
  | DiscardPayment [Target]
  | DiscardCardPayment [Card]
  | ExhaustPayment [Target]
  | RemovePayment [Target]
  | ExilePayment [Target]
  | UsesPayment Int
  | HorrorPayment Int
  | DamagePayment Int
  | SkillIconPayment [SkillType]
  | Payments [Payment]
  | NoPayment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cost
  = ActionCost Int
  | AdditionalActionsCost
  | ClueCost Int
  | GroupClueCost (GameValue Int) (Maybe LocationMatcher)
  | PlaceClueOnLocationCost Int
  | ExhaustCost Target
  | ExhaustThis
  | ExhaustAssetCost AssetMatcher
  | RemoveCost Target
  | Costs [Cost]
  | DamageCost Source Target Int
  | DirectDamageCost Source Target Int
  | DiscardCost Target
  | DiscardCardCost Card
  | DiscardDrawnCardCost
  | DoomCost Source Target Int
  | ExileCost Target
  | HandDiscardCost Int (Maybe CardType) (HashSet Trait) (HashSet SkillType)
  | SkillIconCost Int (HashSet SkillType)
  | HorrorCost Source Target Int
  | Free
  | ResourceCost Int
  | UseCost AssetId UseType Int
  | UpTo Int Cost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
