module Arkham.Location.Cards.HallOfHeresy
  ( hallOfHeresy
  , HallOfHeresy(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HallOfHeresy = HallOfHeresy LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfHeresy :: LocationCard HallOfHeresy
hallOfHeresy =
  symbolLabel $ location HallOfHeresy Cards.hallOfHeresy 4 (PerPlayer 1)

instance HasAbilities HallOfHeresy where
  getAbilities (HallOfHeresy attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage HallOfHeresy where
  runMessage msg (HallOfHeresy attrs) = HallOfHeresy <$> runMessage msg attrs
