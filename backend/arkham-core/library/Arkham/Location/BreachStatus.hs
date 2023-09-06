module Arkham.Location.BreachStatus where

import Arkham.Prelude

data BreachStatus
  = Breaches Int
  | Incursion Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

countBreaches :: BreachStatus -> Int
countBreaches (Breaches n) = n
countBreaches (Incursion n) = n

isIncursion :: BreachStatus -> Bool
isIncursion (Incursion _) = True
isIncursion _ = False

resetIncursion :: BreachStatus -> BreachStatus
resetIncursion (Incursion n) = Breaches n
resetIncursion x = x

addBreach :: Maybe BreachStatus -> Maybe BreachStatus
addBreach Nothing = Just (Breaches 1)
addBreach (Just (Breaches n)) = Just (Breaches (n + 1))
addBreach (Just (Incursion n)) = Just (Incursion (n + 1))

removeBreach :: Maybe BreachStatus -> Maybe BreachStatus
removeBreach Nothing = Just (Breaches 0)
removeBreach (Just (Breaches n)) = Just (Breaches (max 0 (n - 1)))
removeBreach (Just (Incursion n)) = Just (Incursion (max 0 (n - 1)))
