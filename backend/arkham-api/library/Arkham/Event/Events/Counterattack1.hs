module Arkham.Event.Events.Counterattack1 (counterattack1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getAttackDetails)

newtype Counterattack1 = Counterattack1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterattack1 :: EventCard Counterattack1
counterattack1 = event Counterattack1 Cards.counterattack1

instance RunMessage Counterattack1 where
  runMessage msg e@(Counterattack1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let details = getAttackDetails attrs.windows
      cancelAttack attrs details
      nonAttackEnemyDamage (Just iid) attrs 1 details.enemy
      pure e
    _ -> Counterattack1 <$> liftRunMessage msg attrs
