module Arkham.Location.Cards.SerpentsHaven
  ( serpentsHaven
  , SerpentsHaven(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SerpentsHaven = SerpentsHaven LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsHaven :: LocationCard SerpentsHaven
serpentsHaven = location
  SerpentsHaven
  Cards.serpentsHaven
  2
  (PerPlayer 2)
  Triangle
  [Squiggle, Square, Diamond, Hourglass]

instance HasAbilities SerpentsHaven where
  getAbilities (SerpentsHaven attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SerpentsHaven where
  runMessage msg (SerpentsHaven attrs) =
    SerpentsHaven <$> runMessage msg attrs
