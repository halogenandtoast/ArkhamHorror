module Arkham.Location.Cards.WitchHauntedWoodsHermitsHouse
  ( witchHauntedWoodsHermitsHouse
  , WitchHauntedWoodsHermitsHouse(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsHermitsHouse = WitchHauntedWoodsHermitsHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsHermitsHouse :: LocationCard WitchHauntedWoodsHermitsHouse
witchHauntedWoodsHermitsHouse = location WitchHauntedWoodsHermitsHouse Cards.witchHauntedWoodsHermitsHouse 4 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsHermitsHouse where
  getAbilities (WitchHauntedWoodsHermitsHouse attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsHermitsHouse where
  runMessage msg (WitchHauntedWoodsHermitsHouse attrs) =
    WitchHauntedWoodsHermitsHouse <$> runMessage msg attrs
