module Arkham.Location.Cards.WellOfSouls
  ( wellOfSouls
  , WellOfSouls(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype WellOfSouls = WellOfSouls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellOfSouls :: LocationCard WellOfSouls
wellOfSouls = location WellOfSouls Cards.wellOfSouls 0 (Static 0) NoSymbol []

instance HasAbilities WellOfSouls where
  getAbilities (WellOfSouls attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env WellOfSouls where
  runMessage msg (WellOfSouls attrs) =
    WellOfSouls <$> runMessage msg attrs
