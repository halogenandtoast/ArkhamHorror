module Arkham.Location.Cards.FishGraveyard (
  fishGraveyard,
  FishGraveyard (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype FishGraveyard = FishGraveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishGraveyard :: LocationCard FishGraveyard
fishGraveyard = locationWith FishGraveyard Cards.fishGraveyard 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities FishGraveyard where
  getAbilities (FishGraveyard attrs) =
    extendRevealed attrs []

instance RunMessage FishGraveyard where
  runMessage msg (FishGraveyard attrs) = runQueueT $ case msg of
    _ -> FishGraveyard <$> liftRunMessage msg attrs
