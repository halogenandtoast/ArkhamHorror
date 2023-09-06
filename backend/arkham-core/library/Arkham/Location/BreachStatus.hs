module Arkham.Location.BreachStatus where

import Arkham.Prelude

data BreachStatus = Breaches Int | Incursion
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

countBreaches :: BreachStatus -> Int
countBreaches (Breaches n) = n
countBreaches Incursion = 0
