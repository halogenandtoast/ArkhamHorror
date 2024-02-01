module Arkham.Event.Cards.EmergencyCache (
  emergencyCache,
  EmergencyCache (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype EmergencyCache = EmergencyCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

emergencyCache :: EventCard EmergencyCache
emergencyCache = event EmergencyCache Cards.emergencyCache

instance RunMessage EmergencyCache where
  runMessage msg e@(EmergencyCache attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      push $ TakeResources iid 3 (toSource attrs) False
      pure e
    _ -> EmergencyCache <$> runMessage msg attrs
