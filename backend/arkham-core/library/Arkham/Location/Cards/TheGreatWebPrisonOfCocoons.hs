module Arkham.Location.Cards.TheGreatWebPrisonOfCocoons (
  theGreatWebPrisonOfCocoons,
  TheGreatWebPrisonOfCocoons (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebPrisonOfCocoons = TheGreatWebPrisonOfCocoons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebPrisonOfCocoons :: LocationCard TheGreatWebPrisonOfCocoons
theGreatWebPrisonOfCocoons =
  locationWith
    TheGreatWebPrisonOfCocoons
    Cards.theGreatWebPrisonOfCocoons
    4
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebPrisonOfCocoons where
  getAbilities (TheGreatWebPrisonOfCocoons attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebPrisonOfCocoons where
  runMessage msg (TheGreatWebPrisonOfCocoons attrs) = runQueueT $ case msg of
    _ -> TheGreatWebPrisonOfCocoons <$> lift (runMessage msg attrs)
