module Arkham.Location.Cards.WitchHauntedWoodsTaintedWell
  ( witchHauntedWoodsTaintedWell
  , WitchHauntedWoodsTaintedWell(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype WitchHauntedWoodsTaintedWell = WitchHauntedWoodsTaintedWell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsTaintedWell :: LocationCard WitchHauntedWoodsTaintedWell
witchHauntedWoodsTaintedWell = location WitchHauntedWoodsTaintedWell Cards.witchHauntedWoodsTaintedWell 3 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsTaintedWell where
  getAbilities (WitchHauntedWoodsTaintedWell attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WitchHauntedWoodsTaintedWell where
  runMessage msg (WitchHauntedWoodsTaintedWell attrs) =
    WitchHauntedWoodsTaintedWell <$> runMessage msg attrs
