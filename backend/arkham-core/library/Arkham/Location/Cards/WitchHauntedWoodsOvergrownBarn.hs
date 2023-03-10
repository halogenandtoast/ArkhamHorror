module Arkham.Location.Cards.WitchHauntedWoodsOvergrownBarn
  ( witchHauntedWoodsOvergrownBarn
  , WitchHauntedWoodsOvergrownBarn(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsOvergrownBarn = WitchHauntedWoodsOvergrownBarn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsOvergrownBarn :: LocationCard WitchHauntedWoodsOvergrownBarn
witchHauntedWoodsOvergrownBarn = location WitchHauntedWoodsOvergrownBarn Cards.witchHauntedWoodsOvergrownBarn 3 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsOvergrownBarn where
  getAbilities (WitchHauntedWoodsOvergrownBarn attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsOvergrownBarn where
  runMessage msg (WitchHauntedWoodsOvergrownBarn attrs) =
    WitchHauntedWoodsOvergrownBarn <$> runMessage msg attrs
