module Arkham.Event.Cards.MindWipe1 (
  mindWipe1,
  MindWipe1 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message

newtype MindWipe1 = MindWipe1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe1 :: EventCard MindWipe1
mindWipe1 = event MindWipe1 Cards.mindWipe1

instance RunMessage MindWipe1 where
  runMessage msg e@(MindWipe1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- selectList $ enemiesColocatedWith iid <> NonEliteEnemy
      pushWhen (notNull enemies)
        $ chooseOne iid
        $ [ targetLabel eid'
            $ [CreateEffect "01068" Nothing (toSource attrs) (toTarget eid')]
          | eid' <- enemies
          ]
      pure e
    _ -> MindWipe1 <$> runMessage msg attrs
