module Arkham.Event.Events.Infighting3 (infighting3, Infighting3 (..)) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Infighting3 = Infighting3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infighting3 :: EventCard Infighting3
infighting3 = event Infighting3 Cards.infighting3

instance RunMessage Infighting3 where
  runMessage msg e@(Infighting3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      phaseModifier attrs iid (CancelAttacksByEnemies (toCard attrs) NonEliteEnemy)
      pure e
    _ -> Infighting3 <$> liftRunMessage msg attrs
