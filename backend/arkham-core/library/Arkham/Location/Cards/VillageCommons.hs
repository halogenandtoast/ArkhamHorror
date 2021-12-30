module Arkham.Location.Cards.VillageCommons
  ( villageCommons
  , VillageCommons(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (villageCommons)
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Attrs

newtype VillageCommons = VillageCommons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

villageCommons :: LocationCard VillageCommons
villageCommons = location
  VillageCommons
  Cards.villageCommons
  3
  (Static 0)
  Plus
  [Square, Circle, Moon]

instance HasAbilities VillageCommons where
  getAbilities (VillageCommons a) =
    withBaseAbilities a $ [locationResignAction a]

instance LocationRunner env => RunMessage env VillageCommons where
  runMessage msg (VillageCommons attrs) =
    VillageCommons <$> runMessage msg attrs
