module Arkham.Event.Cards.Dodge where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message

newtype Dodge = Dodge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dodge :: EventCard Dodge
dodge = event Dodge Cards.dodge

instance RunMessage Dodge where
  runMessage msg e@(Dodge attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
      e <$ pushAll [CancelNext (toSource attrs) AttackMessage]
    _ -> Dodge <$> runMessage msg attrs
