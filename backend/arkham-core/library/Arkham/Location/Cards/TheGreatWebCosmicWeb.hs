module Arkham.Location.Cards.TheGreatWebCosmicWeb (
  theGreatWebCosmicWeb,
  TheGreatWebCosmicWeb (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebCosmicWeb = TheGreatWebCosmicWeb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebCosmicWeb :: LocationCard TheGreatWebCosmicWeb
theGreatWebCosmicWeb =
  locationWith
    TheGreatWebCosmicWeb
    Cards.theGreatWebCosmicWeb
    4
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebCosmicWeb where
  getAbilities (TheGreatWebCosmicWeb attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebCosmicWeb where
  runMessage msg (TheGreatWebCosmicWeb attrs) = runQueueT $ case msg of
    _ -> TheGreatWebCosmicWeb <$> lift (runMessage msg attrs)
