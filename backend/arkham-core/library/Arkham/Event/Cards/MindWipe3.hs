module Arkham.Event.Cards.MindWipe3 where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype MindWipe3 = MindWipe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe3 :: EventCard MindWipe3
mindWipe3 = event MindWipe3 Cards.mindWipe3

instance RunMessage MindWipe3 where
  runMessage msg e@(MindWipe3 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemyIds <- select $ enemiesColocatedWith iid <> NonEliteEnemy
      unless (null enemyIds) $ do
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ targetLabel eid' [phaseModifiers attrs eid' [Blank, DamageDealt (-1), HorrorDealt (-1)]]
            | eid' <- enemyIds
            ]
      pure e
    _ -> MindWipe3 <$> runMessage msg attrs
