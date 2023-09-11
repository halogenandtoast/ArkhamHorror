module Arkham.Event.Cards.BlindingLight2 (
  blindingLight2,
  BlindingLight2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType

newtype BlindingLight2 = BlindingLight2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EventCard BlindingLight2
blindingLight2 = event BlindingLight2 Cards.blindingLight2

instance RunMessage BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ CreateEffect "01069" Nothing (toSource attrs) (toTarget iid)
        , CreateEffect "01069" Nothing (toSource attrs) SkillTestTarget
        , ChooseEvadeEnemy iid (toSource eid) Nothing #willpower AnyEnemy False
        ]
      pure e
    _ -> BlindingLight2 <$> runMessage msg attrs
