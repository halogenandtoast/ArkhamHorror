module Arkham.Location.Cards.MushroomGrove (mushroomGrove) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MushroomGrove = MushroomGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mushroomGrove :: LocationCard MushroomGrove
mushroomGrove = locationWith MushroomGrove Cards.mushroomGrove 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities MushroomGrove where
  getAbilities (MushroomGrove a) =
    extendRevealed a []

instance RunMessage MushroomGrove where
  runMessage msg (MushroomGrove attrs) = runQueueT $ case msg of
    _ -> MushroomGrove <$> liftRunMessage msg attrs
