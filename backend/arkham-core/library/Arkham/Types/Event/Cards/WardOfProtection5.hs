module Arkham.Types.Event.Cards.WardOfProtection5
  ( wardOfProtection5
  , WardOfProtection5(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype WardOfProtection5 = WardOfProtection5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection5 :: EventCard WardOfProtection5
wardOfProtection5 = event WardOfProtection5 Cards.wardOfProtection5

instance EventRunner env => RunMessage env WardOfProtection5 where
  runMessage msg e@(WardOfProtection5 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ CancelNext DrawEncounterCardMessage
      , InvestigatorAssignDamage iid (EventSource eid) DamageAny 0 1
      , Discard (EventTarget eid)
      ]
    _ -> WardOfProtection5 <$> runMessage msg attrs
