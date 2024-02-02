module Arkham.Location.BreachStatus where

import Arkham.Prelude

-- The Logic for these are a little goofy because we want to make sure not to
-- keep adding breaches from other incursion if an incursion has happened
-- during the phase. We represent this with two constructors and reset during
-- EndPhase. Logic for this is split amongst the Location runner and The
-- Chariot VII

data BreachStatus
  = Breaches Int
  | Incursion Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

countBreaches :: BreachStatus -> Int
countBreaches (Breaches n) = n
countBreaches (Incursion n) = n

isIncursion :: BreachStatus -> Bool
isIncursion (Incursion _) = True
isIncursion _ = False

resetIncursion :: BreachStatus -> BreachStatus
resetIncursion (Incursion n) = Breaches n
resetIncursion x = x

addBreaches :: Int -> Maybe BreachStatus -> Maybe BreachStatus
addBreaches n Nothing = Just (Breaches n)
addBreaches n (Just (Breaches m)) = Just (Breaches (n + m))
addBreaches n (Just (Incursion m)) = Just (Incursion (n + m))

removeBreaches :: Int -> Maybe BreachStatus -> Maybe BreachStatus
removeBreaches _ Nothing = Just (Breaches 0)
removeBreaches n (Just (Breaches m)) = Just (Breaches (max 0 (m - n)))
removeBreaches n (Just (Incursion m)) = Just (Incursion (max 0 (m - n)))
