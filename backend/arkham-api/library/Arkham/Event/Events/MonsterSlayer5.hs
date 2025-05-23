module Arkham.Event.Events.MonsterSlayer5 (monsterSlayer5) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)

newtype MonsterSlayer5 = MonsterSlayer5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monsterSlayer5 :: EventCard MonsterSlayer5
monsterSlayer5 = event MonsterSlayer5 Cards.monsterSlayer5

instance RunMessage MonsterSlayer5 where
  runMessage msg e@(MonsterSlayer5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseFightEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ \eid -> do
        whenMatch eid NonEliteEnemy (defeatEnemy eid iid attrs)
      pure e
    _ -> MonsterSlayer5 <$> liftRunMessage msg attrs
