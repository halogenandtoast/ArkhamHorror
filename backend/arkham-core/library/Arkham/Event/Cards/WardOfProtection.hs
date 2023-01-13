module Arkham.Event.Cards.WardOfProtection where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype WardOfProtection = WardOfProtection EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection :: EventCard WardOfProtection
wardOfProtection = event WardOfProtection Cards.wardOfProtection

instance RunMessage WardOfProtection where
  runMessage msg e@(WardOfProtection attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ CancelNext (toSource attrs) RevelationMessage
      , InvestigatorAssignDamage iid (EventSource eid) DamageAny 0 1
      , Discard (EventTarget eid)
      ]
    _ -> WardOfProtection <$> runMessage msg attrs
