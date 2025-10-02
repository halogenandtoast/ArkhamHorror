module Arkham.Location.Cards.TheTowerBridge (theTowerBridge) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheTowerBridge = TheTowerBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTowerBridge :: LocationCard TheTowerBridge
theTowerBridge = symbolLabel $ location TheTowerBridge Cards.theTowerBridge 2 (PerPlayer 1)

instance HasAbilities TheTowerBridge where
  getAbilities (TheTowerBridge attrs) =
    extendRevealed attrs []

instance RunMessage TheTowerBridge where
  runMessage msg (TheTowerBridge attrs) = runQueueT $ case msg of
    _ -> TheTowerBridge <$> liftRunMessage msg attrs
