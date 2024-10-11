module Arkham.Event.Events.MindWipe1 (mindWipe1, MindWipe1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Modifier

newtype MindWipe1 = MindWipe1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe1 :: EventCard MindWipe1
mindWipe1 = event MindWipe1 Cards.mindWipe1

instance RunMessage MindWipe1 where
  runMessage msg e@(MindWipe1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      enemies <- select $ enemiesColocatedWith iid <> NonEliteEnemy
      chooseTargetM iid enemies \t -> phaseModifier attrs t Blank
      pure e
    _ -> MindWipe1 <$> liftRunMessage msg attrs
