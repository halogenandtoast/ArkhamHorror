module Arkham.Location.Cards.WitchHouseRuins
  ( witchHouseRuins
  , WitchHouseRuins(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype WitchHouseRuins = WitchHouseRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHouseRuins :: LocationCard WitchHouseRuins
witchHouseRuins = location WitchHouseRuins Cards.witchHouseRuins 2 (Static 0)

instance HasAbilities WitchHouseRuins where
  getAbilities (WitchHouseRuins attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHouseRuins where
  runMessage msg (WitchHouseRuins attrs) =
    WitchHouseRuins <$> runMessage msg attrs
