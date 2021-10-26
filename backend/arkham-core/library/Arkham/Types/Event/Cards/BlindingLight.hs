module Arkham.Types.Event.Cards.BlindingLight where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards (blindingLight)
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype BlindingLight = BlindingLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EventCard BlindingLight
blindingLight = event BlindingLight Cards.blindingLight

instance EventRunner env => RunMessage env BlindingLight where
  runMessage msg e@(BlindingLight attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ CreateEffect "01066" Nothing (toSource attrs) (InvestigatorTarget iid)
      , CreateEffect "01066" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy
        iid
        (EventSource eid)
        Nothing
        SkillWillpower
        AnyEnemy
        False
      , Discard (EventTarget eid)
      ]
    _ -> BlindingLight <$> runMessage msg attrs
