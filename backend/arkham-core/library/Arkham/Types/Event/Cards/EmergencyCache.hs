module Arkham.Types.Event.Cards.EmergencyCache where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Event.Attrs

newtype EmergencyCache = EmergencyCache EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache :: InvestigatorId -> EventId -> EmergencyCache
emergencyCache iid uuid = EmergencyCache $ baseAttrs iid uuid "01088"

instance HasModifiersFor env EmergencyCache where
  getModifiersFor = noModifiersFor

instance HasActions env EmergencyCache where
  getActions i window (EmergencyCache attrs) = getActions i window attrs

instance HasQueue env => RunMessage env EmergencyCache where
  runMessage msg e@(EmergencyCache attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId ->
      e <$ unshiftMessages
        [TakeResources iid 3 False, Discard (EventTarget eid)]
    _ -> EmergencyCache <$> runMessage msg attrs
