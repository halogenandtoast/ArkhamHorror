module Arkham.Types.Cost
  ( module Arkham.Types.Cost
  )
where

import Arkham.Prelude

import Arkham.Types.Card.PlayerCard
import Arkham.Types.Trait

totalActions :: Cost -> Int
totalActions (ActionCost n) = n
totalActions (Costs xs) = sum $ map totalActions xs
totalActions _ = 0

decreaseActions :: Cost -> Int -> Cost
decreaseActions (ActionCost x) y = ActionCost $ max 0 (x - y)
decreaseActions (Costs (a : as)) y = case a of
  ActionCost x | x >= y -> Costs (ActionCost (x - y) : as)
  ActionCost x ->
    ActionCost (max 0 (x - y)) <> decreaseActions (Costs as) (y - x)
  _ -> a <> decreaseActions (Costs as) y
decreaseActions other _ = other

data Cost
  = ActionCost Int
  | DiscardCost Int (Maybe PlayerCardType) (HashSet Trait)
  | ClueCost Int
  | ResourceCost Int
  | Costs [Cost]
  | Free
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Cost where
  Costs xs <> Costs ys = Costs (xs <> ys)
  Costs xs <> a = Costs (a : xs)
  a <> Costs xs = Costs (a : xs)
  a <> b = Costs [a, b]
