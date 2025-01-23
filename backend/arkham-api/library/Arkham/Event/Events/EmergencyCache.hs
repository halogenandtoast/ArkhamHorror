module Arkham.Event.Events.EmergencyCache (emergencyCache) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype EmergencyCache = EmergencyCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache :: EventCard EmergencyCache
emergencyCache = event EmergencyCache Cards.emergencyCache

instance RunMessage EmergencyCache where
  runMessage msg e@(EmergencyCache attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResources iid attrs 3
      pure e
    _ -> EmergencyCache <$> liftRunMessage msg attrs
