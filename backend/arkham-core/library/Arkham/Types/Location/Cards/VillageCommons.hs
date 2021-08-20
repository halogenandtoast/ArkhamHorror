module Arkham.Types.Location.Cards.VillageCommons
  ( villageCommons
  , VillageCommons(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (villageCommons)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype VillageCommons = VillageCommons LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

villageCommons :: LocationCard VillageCommons
villageCommons = location
  VillageCommons
  Cards.villageCommons
  3
  (Static 0)
  Plus
  [Square, Circle, Moon]

instance HasModifiersFor env VillageCommons

instance HasAbilities env VillageCommons where
  getAbilities = withResignAction

instance LocationRunner env => RunMessage env VillageCommons where
  runMessage msg (VillageCommons attrs) =
    VillageCommons <$> runMessage msg attrs
