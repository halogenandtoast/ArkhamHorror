module Arkham.Types.Cost
  ( module Arkham.Types.Cost
  )
where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.LocationMatcher
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

data Cost
  = ActionCost Int
  | ClueCost Int
  | GroupClueCost Int (Maybe LocationMatcher)
  | ExhaustCost Target
  | Costs [Cost]
  | DiscardCost Target
  | DiscardCardCost CardId
  | HandDiscardCost Int (Maybe PlayerCardType) (HashSet Trait)
  | HorrorCost Source Target Int
  | Free
  | ResourceCost Int
  | UseCost AssetId UseType Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Cost where
  Costs xs <> Costs ys = Costs (xs <> ys)
  Costs xs <> a = Costs (a : xs)
  a <> Costs xs = Costs (a : xs)
  a <> b = Costs [a, b]
