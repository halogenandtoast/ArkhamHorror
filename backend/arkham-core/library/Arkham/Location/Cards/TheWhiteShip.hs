module Arkham.Location.Cards.TheWhiteShip
  ( theWhiteShip
  , TheWhiteShip(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheWhiteShip = TheWhiteShip LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWhiteShip :: LocationCard TheWhiteShip
theWhiteShip = location TheWhiteShip Cards.theWhiteShip 0 (Static 0)

instance HasAbilities TheWhiteShip where
  getAbilities (TheWhiteShip attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheWhiteShip where
  runMessage msg (TheWhiteShip attrs) =
    TheWhiteShip <$> runMessage msg attrs
