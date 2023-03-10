module Arkham.Location.Cards.WitchHauntedWoodsAbandonedMine
  ( witchHauntedWoodsAbandonedMine
  , WitchHauntedWoodsAbandonedMine(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsAbandonedMine = WitchHauntedWoodsAbandonedMine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsAbandonedMine :: LocationCard WitchHauntedWoodsAbandonedMine
witchHauntedWoodsAbandonedMine = location WitchHauntedWoodsAbandonedMine Cards.witchHauntedWoodsAbandonedMine 2 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsAbandonedMine where
  getAbilities (WitchHauntedWoodsAbandonedMine attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsAbandonedMine where
  runMessage msg (WitchHauntedWoodsAbandonedMine attrs) =
    WitchHauntedWoodsAbandonedMine <$> runMessage msg attrs
