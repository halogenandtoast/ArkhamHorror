module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheEyeOfRavens (theEyeOfRavens) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted

newtype TheEyeOfRavens = TheEyeOfRavens ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEyeOfRavens :: ScarletKeyCard TheEyeOfRavens
theEyeOfRavens = key TheEyeOfRavens Cards.theEyeOfRavens

instance RunMessage TheEyeOfRavens where
  runMessage msg (TheEyeOfRavens attrs) = runQueueT $ case msg of
    _ -> TheEyeOfRavens <$> liftRunMessage msg attrs
