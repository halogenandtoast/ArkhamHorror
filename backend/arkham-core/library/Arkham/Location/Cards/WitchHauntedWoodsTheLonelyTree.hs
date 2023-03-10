module Arkham.Location.Cards.WitchHauntedWoodsTheLonelyTree
  ( witchHauntedWoodsTheLonelyTree
  , WitchHauntedWoodsTheLonelyTree(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsTheLonelyTree = WitchHauntedWoodsTheLonelyTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsTheLonelyTree :: LocationCard WitchHauntedWoodsTheLonelyTree
witchHauntedWoodsTheLonelyTree = location WitchHauntedWoodsTheLonelyTree Cards.witchHauntedWoodsTheLonelyTree 2 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsTheLonelyTree where
  getAbilities (WitchHauntedWoodsTheLonelyTree attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsTheLonelyTree where
  runMessage msg (WitchHauntedWoodsTheLonelyTree attrs) =
    WitchHauntedWoodsTheLonelyTree <$> runMessage msg attrs
