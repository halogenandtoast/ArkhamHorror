module Arkham.Event.Events.ManoAMano2 (manoAMano2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype ManoAMano2 = ManoAMano2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manoAMano2 :: EventCard ManoAMano2
manoAMano2 = event ManoAMano2 Cards.manoAMano2

instance RunMessage ManoAMano2 where
  runMessage msg e@(ManoAMano2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseDamageEnemy iid attrs Anywhere (enemyEngagedWith iid) 2
      pure e
    _ -> ManoAMano2 <$> liftRunMessage msg attrs
