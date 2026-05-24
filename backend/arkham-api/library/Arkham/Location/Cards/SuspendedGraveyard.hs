module Arkham.Location.Cards.SuspendedGraveyard (suspendedGraveyard) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SuspendedGraveyard = SuspendedGraveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspendedGraveyard :: LocationCard SuspendedGraveyard
suspendedGraveyard = locationWith SuspendedGraveyard Cards.suspendedGraveyard 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities SuspendedGraveyard where
  getAbilities (SuspendedGraveyard a) =
    extendRevealed a []

instance RunMessage SuspendedGraveyard where
  runMessage msg (SuspendedGraveyard attrs) = runQueueT $ case msg of
    _ -> SuspendedGraveyard <$> liftRunMessage msg attrs
