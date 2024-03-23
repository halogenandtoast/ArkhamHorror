module Arkham.Location.Cards.TheGreatWebWebStairs (
  theGreatWebWebStairs,
  TheGreatWebWebStairs (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebWebStairs = TheGreatWebWebStairs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebWebStairs :: LocationCard TheGreatWebWebStairs
theGreatWebWebStairs =
  locationWith
    TheGreatWebWebStairs
    Cards.theGreatWebWebStairs
    5
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebWebStairs where
  getAbilities (TheGreatWebWebStairs attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebWebStairs where
  runMessage msg (TheGreatWebWebStairs attrs) = runQueueT $ case msg of
    _ -> TheGreatWebWebStairs <$> lift (runMessage msg attrs)
