module Arkham.Event.Events.EmergencyCache (emergencyCache) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (gainResources)
import Arkham.Script

newtype EmergencyCache = EmergencyCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache :: EventCard EmergencyCache
emergencyCache = event EmergencyCache Cards.emergencyCache

instance RunMessage EmergencyCache where
  runMessage = script $ onPlay $ gainResources you 3
