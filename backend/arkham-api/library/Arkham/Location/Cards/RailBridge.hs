module Arkham.Location.Cards.RailBridge (railBridge) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RailBridge = RailBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

railBridge :: LocationCard RailBridge
railBridge = location RailBridge Cards.railBridge 0 (PerPlayer 2)

instance HasAbilities RailBridge where
  getAbilities (RailBridge a) =
    extendRevealed a []

instance RunMessage RailBridge where
  runMessage msg (RailBridge attrs) = runQueueT $ case msg of
    _ -> RailBridge <$> liftRunMessage msg attrs
