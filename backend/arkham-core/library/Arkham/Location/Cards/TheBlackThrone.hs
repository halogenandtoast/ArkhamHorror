module Arkham.Location.Cards.TheBlackThrone (
  theBlackThrone,
  TheBlackThrone (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype TheBlackThrone = TheBlackThrone LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackThrone :: LocationCard TheBlackThrone
theBlackThrone =
  locationWith
    TheBlackThrone
    Cards.theBlackThrone
    1
    (PerPlayer 2)
    (connectsToL .~ adjacentLocations)

instance HasAbilities TheBlackThrone where
  getAbilities (TheBlackThrone attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TheBlackThrone where
  runMessage msg (TheBlackThrone attrs) =
    TheBlackThrone <$> runMessage msg attrs
