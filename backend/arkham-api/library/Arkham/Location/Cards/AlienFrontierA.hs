module Arkham.Location.Cards.AlienFrontierA (alienFrontierA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlienFrontierA = AlienFrontierA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFrontierA :: LocationCard AlienFrontierA
alienFrontierA = symbolLabel $ locationWith AlienFrontierA Cards.alienFrontierA 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AlienFrontierA where
  getAbilities (AlienFrontierA a) =
    extendRevealed a []

instance RunMessage AlienFrontierA where
  runMessage msg (AlienFrontierA attrs) = runQueueT $ case msg of
    _ -> AlienFrontierA <$> liftRunMessage msg attrs
