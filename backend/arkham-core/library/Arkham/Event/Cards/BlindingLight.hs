module Arkham.Event.Cards.BlindingLight where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards (blindingLight)
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype BlindingLight = BlindingLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EventCard BlindingLight
blindingLight = event BlindingLight Cards.blindingLight

instance RunMessage BlindingLight where
  runMessage msg e@(BlindingLight attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ CreateEffect "01066" Nothing (toSource attrs) (toTarget iid)
        , CreateEffect "01066" Nothing (toSource attrs) SkillTestTarget
        , ChooseEvadeEnemy iid (toSource eid) Nothing #willpower AnyEnemy False
        ]
      pure e
    _ -> BlindingLight <$> runMessage msg attrs
