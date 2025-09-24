module Arkham.Location.Cards.WitchHauntedWoodsUnmarkedGraveyard (witchHauntedWoodsUnmarkedGraveyard) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WitchHauntedWoodsUnmarkedGraveyard = WitchHauntedWoodsUnmarkedGraveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsUnmarkedGraveyard :: LocationCard WitchHauntedWoodsUnmarkedGraveyard
witchHauntedWoodsUnmarkedGraveyard = location WitchHauntedWoodsUnmarkedGraveyard Cards.witchHauntedWoodsUnmarkedGraveyard 1 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsUnmarkedGraveyard where
  getAbilities (WitchHauntedWoodsUnmarkedGraveyard attrs) =
    extendRevealed attrs []

instance RunMessage WitchHauntedWoodsUnmarkedGraveyard where
  runMessage msg (WitchHauntedWoodsUnmarkedGraveyard attrs) = runQueueT $ case msg of
    _ -> WitchHauntedWoodsUnmarkedGraveyard <$> liftRunMessage msg attrs
