module Arkham.Location.Cards.TempleOfTheFang
  ( templeOfTheFang
  , TempleOfTheFang(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TempleOfTheFang = TempleOfTheFang LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheFang :: LocationCard TempleOfTheFang
templeOfTheFang = location
  TempleOfTheFang
  Cards.templeOfTheFang
  2
  (PerPlayer 1)
  Squiggle
  [Square, Triangle, Equals]

instance HasAbilities TempleOfTheFang where
  getAbilities (TempleOfTheFang attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TempleOfTheFang where
  runMessage msg (TempleOfTheFang attrs) =
    TempleOfTheFang <$> runMessage msg attrs
