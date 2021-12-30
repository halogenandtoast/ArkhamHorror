module Arkham.Event.Cards.Lucky where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Target

newtype Lucky = Lucky EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky :: EventCard Lucky
lucky = event Lucky Cards.lucky

instance EventRunner env => RunMessage env Lucky where
  runMessage msg e@(Lucky attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId ->
      e <$ unshiftEffect attrs (InvestigatorTarget iid)
    _ -> Lucky <$> runMessage msg attrs
