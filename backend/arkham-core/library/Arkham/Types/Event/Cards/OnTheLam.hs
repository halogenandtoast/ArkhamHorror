module Arkham.Types.Event.Cards.OnTheLam where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype OnTheLam = OnTheLam EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EventCard OnTheLam
onTheLam = event OnTheLam Cards.onTheLam

instance HasModifiersFor env OnTheLam where
  getModifiersFor = noModifiersFor

instance HasActions env OnTheLam where
  getActions i window (OnTheLam attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env OnTheLam where
  runMessage msg e@(OnTheLam attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftEffect attrs (InvestigatorTarget iid)
    _ -> OnTheLam <$> runMessage msg attrs
