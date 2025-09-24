module Arkham.Location.Cards.WitchHauntedWoodsWitchTree (witchHauntedWoodsWitchTree) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WitchHauntedWoodsWitchTree = WitchHauntedWoodsWitchTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsWitchTree :: LocationCard WitchHauntedWoodsWitchTree
witchHauntedWoodsWitchTree = location WitchHauntedWoodsWitchTree Cards.witchHauntedWoodsWitchTree 4 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsWitchTree where
  getAbilities (WitchHauntedWoodsWitchTree attrs) =
    extendRevealed attrs []

instance RunMessage WitchHauntedWoodsWitchTree where
  runMessage msg (WitchHauntedWoodsWitchTree attrs) = runQueueT $ case msg of
    _ -> WitchHauntedWoodsWitchTree <$> liftRunMessage msg attrs
