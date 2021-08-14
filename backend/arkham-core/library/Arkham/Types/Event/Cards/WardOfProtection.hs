module Arkham.Types.Event.Cards.WardOfProtection where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype WardOfProtection = WardOfProtection EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection :: EventCard WardOfProtection
wardOfProtection = event WardOfProtection Cards.wardOfProtection

instance EventRunner env => RunMessage env WardOfProtection where
  runMessage msg e@(WardOfProtection attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ CancelNext RevelationMessage
      , InvestigatorAssignDamage iid (EventSource eid) DamageAny 0 1
      , Discard (EventTarget eid)
      ]
    _ -> WardOfProtection <$> runMessage msg attrs
