module Arkham.Location.Cards.WitchHauntedWoodsCairnStones
  ( witchHauntedWoodsCairnStones
  , WitchHauntedWoodsCairnStones(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsCairnStones = WitchHauntedWoodsCairnStones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsCairnStones :: LocationCard WitchHauntedWoodsCairnStones
witchHauntedWoodsCairnStones = location WitchHauntedWoodsCairnStones Cards.witchHauntedWoodsCairnStones 3 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsCairnStones where
  getAbilities (WitchHauntedWoodsCairnStones attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsCairnStones where
  runMessage msg (WitchHauntedWoodsCairnStones attrs) =
    WitchHauntedWoodsCairnStones <$> runMessage msg attrs
