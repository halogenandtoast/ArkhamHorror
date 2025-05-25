module Arkham.Event.Events.ManoAMano1 (manoAMano1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype ManoAMano1 = ManoAMano1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manoAMano1 :: EventCard ManoAMano1
manoAMano1 = event ManoAMano1 Cards.manoAMano1

instance RunMessage ManoAMano1 where
  runMessage msg e@(ManoAMano1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemyEngagedWith iid
      chooseOrRunOneM iid $ targets enemies (nonAttackEnemyDamage (Just iid) attrs 1)
      pure e
    _ -> ManoAMano1 <$> liftRunMessage msg attrs
