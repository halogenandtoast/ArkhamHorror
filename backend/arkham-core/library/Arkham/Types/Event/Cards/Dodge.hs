module Arkham.Types.Event.Cards.Dodge where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Dodge = Dodge EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dodge :: EventCard Dodge
dodge = event Dodge Cards.dodge

instance HasModifiersFor env Dodge where
  getModifiersFor = noModifiersFor

instance HasActions env Dodge where
  getActions i window (Dodge attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Dodge where
  runMessage msg e@(Dodge attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      e <$ unshiftMessages [CancelNext AttackMessage, Discard (EventTarget eid)]
    _ -> Dodge <$> runMessage msg attrs
