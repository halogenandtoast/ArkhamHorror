module Arkham.Location.Cards.TempleOfUnattainableDesires
  ( templeOfUnattainableDesires
  , TempleOfUnattainableDesires(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TempleOfUnattainableDesires = TempleOfUnattainableDesires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

templeOfUnattainableDesires :: LocationCard TempleOfUnattainableDesires
templeOfUnattainableDesires = location TempleOfUnattainableDesires Cards.templeOfUnattainableDesires 3 (PerPlayer 1)

instance HasAbilities TempleOfUnattainableDesires where
  getAbilities (TempleOfUnattainableDesires attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TempleOfUnattainableDesires where
  runMessage msg (TempleOfUnattainableDesires attrs) =
    TempleOfUnattainableDesires <$> runMessage msg attrs
