module Arkham.Event.Events.MindWipe3 where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Modifier

newtype MindWipe3 = MindWipe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe3 :: EventCard MindWipe3
mindWipe3 = event MindWipe3 Cards.mindWipe3

instance RunMessage MindWipe3 where
  runMessage msg e@(MindWipe3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <- select $ enemiesColocatedWith iid <> NonEliteEnemy
      chooseTargetM iid enemies \eid' -> phaseModifiers attrs eid' [Blank, DamageDealt (-1), HorrorDealt (-1)]
      pure e
    _ -> MindWipe3 <$> liftRunMessage msg attrs
