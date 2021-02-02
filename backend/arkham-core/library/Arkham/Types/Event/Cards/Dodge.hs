module Arkham.Types.Event.Cards.Dodge where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Dodge = Dodge Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

dodge :: InvestigatorId -> EventId -> Dodge
dodge iid uuid = Dodge $ baseAttrs iid uuid "01023"

instance HasModifiersFor env Dodge where
  getModifiersFor = noModifiersFor

instance HasActions env Dodge where
  getActions i window (Dodge attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Dodge where
  runMessage msg e@(Dodge attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      e <$ unshiftMessages [CancelNext AttackMessage, Discard (EventTarget eid)]
    _ -> Dodge <$> runMessage msg attrs
