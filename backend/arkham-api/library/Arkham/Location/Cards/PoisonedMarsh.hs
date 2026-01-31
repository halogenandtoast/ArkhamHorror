module Arkham.Location.Cards.PoisonedMarsh (poisonedMarsh) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PoisonedMarsh = PoisonedMarsh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonedMarsh :: LocationCard PoisonedMarsh
poisonedMarsh = locationWith PoisonedMarsh Cards.poisonedMarsh 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities PoisonedMarsh where
  getAbilities (PoisonedMarsh a) =
    extendRevealed a []

instance RunMessage PoisonedMarsh where
  runMessage msg (PoisonedMarsh attrs) = runQueueT $ case msg of
    _ -> PoisonedMarsh <$> liftRunMessage msg attrs
