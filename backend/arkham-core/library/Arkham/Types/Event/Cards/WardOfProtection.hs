module Arkham.Types.Event.Cards.WardOfProtection where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype WardOfProtection = WardOfProtection EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection :: InvestigatorId -> EventId -> WardOfProtection
wardOfProtection iid uuid = WardOfProtection $ baseAttrs iid uuid "01065"

instance HasModifiersFor env WardOfProtection where
  getModifiersFor = noModifiersFor

instance HasActions env WardOfProtection where
  getActions i window (WardOfProtection attrs) = getActions i window attrs

instance EventRunner env => RunMessage env WardOfProtection where
  runMessage msg e@(WardOfProtection attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ CancelNext RevelationMessage
      , InvestigatorAssignDamage iid (EventSource eid) DamageAny 0 1
      , Discard (EventTarget eid)
      ]
    _ -> WardOfProtection <$> runMessage msg attrs
