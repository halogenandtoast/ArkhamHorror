module Arkham.Location.Cards.AlienFrontierB (alienFrontierB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlienFrontierB = AlienFrontierB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFrontierB :: LocationCard AlienFrontierB
alienFrontierB = symbolLabel $ locationWith AlienFrontierB Cards.alienFrontierB 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AlienFrontierB where
  getAbilities (AlienFrontierB a) =
    extendRevealed a []

instance RunMessage AlienFrontierB where
  runMessage msg (AlienFrontierB attrs) = runQueueT $ case msg of
    _ -> AlienFrontierB <$> liftRunMessage msg attrs
