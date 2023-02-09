module Arkham.Event.Cards.MindWipe3 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype MindWipe3 = MindWipe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe3 :: EventCard MindWipe3
mindWipe3 = event MindWipe3 Cards.mindWipe3

instance RunMessage MindWipe3 where
  runMessage msg e@(MindWipe3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <- selectList $ enemiesColocatedWith iid <> NonEliteEnemy
      if null enemyIds
        then push (discard attrs)
        else pushAll
          [ chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid')
                [CreateEffect "" Nothing (toSource attrs) (EnemyTarget eid')]
            | eid' <- enemyIds
            ]
          , discard attrs
          ]
      pure e
    _ -> MindWipe3 <$> runMessage msg attrs
