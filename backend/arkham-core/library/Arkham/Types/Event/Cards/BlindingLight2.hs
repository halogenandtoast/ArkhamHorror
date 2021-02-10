module Arkham.Types.Event.Cards.BlindingLight2 where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype BlindingLight2 = BlindingLight2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: InvestigatorId -> EventId -> BlindingLight2
blindingLight2 iid uuid = BlindingLight2 $ baseAttrs iid uuid "01069"

instance HasModifiersFor env BlindingLight2 where
  getModifiersFor = noModifiersFor

instance HasActions env BlindingLight2 where
  getActions i window (BlindingLight2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ CreateEffect "01069" Nothing (toSource attrs) (InvestigatorTarget iid)
      , CreateEffect "01069" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy iid (EventSource eid) SkillWillpower False
      , Discard (EventTarget eid)
      ]
    _ -> BlindingLight2 <$> runMessage msg attrs
