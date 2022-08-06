module Arkham.Location.Cards.VillageCommons
  ( villageCommons
  , VillageCommons(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( villageCommons )
import Arkham.Location.Runner

newtype VillageCommons = VillageCommons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

villageCommons :: LocationCard VillageCommons
villageCommons = location VillageCommons Cards.villageCommons 3 (Static 0)

instance HasAbilities VillageCommons where
  getAbilities (VillageCommons a) =
    withBaseAbilities a $ [locationResignAction a]

instance RunMessage VillageCommons where
  runMessage msg (VillageCommons attrs) =
    VillageCommons <$> runMessage msg attrs
