module Arkham.Event.Cards.OnTheLam where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Helpers.Event
import Arkham.Message
import Arkham.Target

newtype OnTheLam = OnTheLam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EventCard OnTheLam
onTheLam = event OnTheLam Cards.onTheLam

instance RunMessage OnTheLam where
  runMessage msg e@(OnTheLam attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      e <$ unshiftEffect attrs (InvestigatorTarget iid)
    _ -> OnTheLam <$> runMessage msg attrs
