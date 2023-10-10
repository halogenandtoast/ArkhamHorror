module Arkham.Event.Cards.MindWipe3 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype MindWipe3 = MindWipe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe3 :: EventCard MindWipe3
mindWipe3 = event MindWipe3 Cards.mindWipe3

instance RunMessage MindWipe3 where
  runMessage msg e@(MindWipe3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <- selectList $ enemiesColocatedWith iid <> NonEliteEnemy
      player <- getPlayer iid
      unless (null enemyIds)
        $ pushAll
          [ chooseOne
              player
              [ TargetLabel
                (EnemyTarget eid')
                [CreateEffect "" Nothing (toSource attrs) (EnemyTarget eid')]
              | eid' <- enemyIds
              ]
          ]
      pure e
    _ -> MindWipe3 <$> runMessage msg attrs
