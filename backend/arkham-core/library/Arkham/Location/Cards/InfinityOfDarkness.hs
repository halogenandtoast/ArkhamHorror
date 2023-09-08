module Arkham.Location.Cards.InfinityOfDarkness (
  infinityOfDarkness,
  InfinityOfDarkness (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype InfinityOfDarkness = InfinityOfDarkness LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infinityOfDarkness :: LocationCard InfinityOfDarkness
infinityOfDarkness =
  locationWith
    InfinityOfDarkness
    Cards.infinityOfDarkness
    2
    (Static 1)
    (connectsToL .~ adjacentLocations)

instance HasAbilities InfinityOfDarkness where
  getAbilities (InfinityOfDarkness attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage InfinityOfDarkness where
  runMessage msg (InfinityOfDarkness attrs) =
    InfinityOfDarkness <$> runMessage msg attrs
