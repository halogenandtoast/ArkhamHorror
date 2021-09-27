module Arkham.Types.Event.Cards.MindOverMatter where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype MindOverMatter = MindOverMatter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EventCard MindOverMatter
mindOverMatter = event MindOverMatter Cards.mindOverMatter

instance EventRunner env => RunMessage env MindOverMatter where
  runMessage msg e@(MindOverMatter attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      e <$ pushAll
        [ CreateEffect "01036" Nothing (toSource attrs) (InvestigatorTarget iid)
        , Discard (EventTarget eid)
        ]
    _ -> MindOverMatter <$> runMessage msg attrs
