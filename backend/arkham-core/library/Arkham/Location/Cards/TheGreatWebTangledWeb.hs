module Arkham.Location.Cards.TheGreatWebTangledWeb (
  theGreatWebTangledWeb,
  TheGreatWebTangledWeb (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebTangledWeb = TheGreatWebTangledWeb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebTangledWeb :: LocationCard TheGreatWebTangledWeb
theGreatWebTangledWeb =
  locationWith
    TheGreatWebTangledWeb
    Cards.theGreatWebTangledWeb
    2
    (PerPlayer 2)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebTangledWeb where
  getAbilities (TheGreatWebTangledWeb attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebTangledWeb where
  runMessage msg (TheGreatWebTangledWeb attrs) = runQueueT $ case msg of
    _ -> TheGreatWebTangledWeb <$> lift (runMessage msg attrs)
