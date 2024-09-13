module Arkham.Location.Cards.BoneRiddenPit (
  boneRiddenPit,
  BoneRiddenPit (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype BoneRiddenPit = BoneRiddenPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boneRiddenPit :: LocationCard BoneRiddenPit
boneRiddenPit = locationWith BoneRiddenPit Cards.boneRiddenPit 6 (PerPlayer 1) connectsToAdjacent

instance HasAbilities BoneRiddenPit where
  getAbilities (BoneRiddenPit attrs) =
    extendRevealed attrs []

instance RunMessage BoneRiddenPit where
  runMessage msg (BoneRiddenPit attrs) = runQueueT $ case msg of
    _ -> BoneRiddenPit <$> liftRunMessage msg attrs
