module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheLastBlossom (theLastBlossom) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheLastBlossom = TheLastBlossom ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLastBlossom :: ScarletKeyCard TheLastBlossom
theLastBlossom = key TheLastBlossom Cards.theLastBlossom

instance RunMessage TheLastBlossom where
  runMessage msg (TheLastBlossom attrs) = runQueueT $ case msg of
    _ -> TheLastBlossom <$> liftRunMessage msg attrs
