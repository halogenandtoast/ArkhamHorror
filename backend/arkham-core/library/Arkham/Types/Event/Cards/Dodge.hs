module Arkham.Types.Event.Cards.Dodge where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype Dodge = Dodge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dodge :: EventCard Dodge
dodge = event Dodge Cards.dodge

instance EventRunner env => RunMessage env Dodge where
  runMessage msg e@(Dodge attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ _ | eid == eventId -> do
      e <$ pushAll [CancelNext AttackMessage, Discard (EventTarget eid)]
    _ -> Dodge <$> runMessage msg attrs
