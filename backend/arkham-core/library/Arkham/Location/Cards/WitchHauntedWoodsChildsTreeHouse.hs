module Arkham.Location.Cards.WitchHauntedWoodsChildsTreeHouse
  ( witchHauntedWoodsChildsTreeHouse
  , WitchHauntedWoodsChildsTreeHouse(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsChildsTreeHouse = WitchHauntedWoodsChildsTreeHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsChildsTreeHouse :: LocationCard WitchHauntedWoodsChildsTreeHouse
witchHauntedWoodsChildsTreeHouse = location WitchHauntedWoodsChildsTreeHouse Cards.witchHauntedWoodsChildsTreeHouse 1 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsChildsTreeHouse where
  getAbilities (WitchHauntedWoodsChildsTreeHouse attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsChildsTreeHouse where
  runMessage msg (WitchHauntedWoodsChildsTreeHouse attrs) =
    WitchHauntedWoodsChildsTreeHouse <$> runMessage msg attrs
