module Arkham.Event.Cards.Lucky where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Helpers.Event
import Arkham.Message
import Arkham.Target

newtype Lucky = Lucky EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky :: EventCard Lucky
lucky = event Lucky Cards.lucky

instance RunMessage Lucky where
  runMessage msg e@(Lucky attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId ->
      e <$ unshiftEffect attrs (InvestigatorTarget iid)
    _ -> Lucky <$> runMessage msg attrs
