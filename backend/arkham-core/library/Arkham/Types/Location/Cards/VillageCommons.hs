module Arkham.Types.Location.Cards.VillageCommons
  ( villageCommons
  , VillageCommons(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (villageCommons)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

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

instance HasAbilities env VillageCommons where
  getAbilities iid window (VillageCommons a) =
    withBaseAbilities iid window a $ pure [locationResignAction a]

instance LocationRunner env => RunMessage env VillageCommons where
  runMessage msg (VillageCommons attrs) =
    VillageCommons <$> runMessage msg attrs
