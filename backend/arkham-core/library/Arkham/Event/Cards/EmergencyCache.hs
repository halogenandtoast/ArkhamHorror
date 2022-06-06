module Arkham.Event.Cards.EmergencyCache
  ( emergencyCache
  , EmergencyCache(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Message
import Arkham.Target

newtype EmergencyCache = EmergencyCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache :: EventCard EmergencyCache
emergencyCache = event EmergencyCache Cards.emergencyCache

instance RunMessage EmergencyCache where
  runMessage msg e@(EmergencyCache attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId ->
      e <$ pushAll [TakeResources iid 3 False, Discard (EventTarget eid)]
    _ -> EmergencyCache <$> runMessage msg attrs
