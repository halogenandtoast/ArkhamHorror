module Arkham.Event.Cards.BlindingLight where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards (blindingLight)
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source

newtype BlindingLight = BlindingLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EventCard BlindingLight
blindingLight = event BlindingLight Cards.blindingLight

instance RunMessage BlindingLight where
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
      ]
    _ -> BlindingLight <$> runMessage msg attrs
