module Arkham.Event.Events.BendBlood5 (bendBlood5) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype BendBlood5 = BendBlood5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bendBlood5 :: EventCard BendBlood5
bendBlood5 = event BendBlood5 Cards.bendBlood5

instance RunMessage BendBlood5 where
  runMessage msg e@(BendBlood5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #willpower 3, DamageDealt 1]
      chooseFightEnemyWith #willpower sid iid attrs
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid (isSource attrs -> True) _ _ -> do
      roundModifiers attrs eid [CannotAttack]
      enemies <-
        select $ enemyAtLocationWith iid <> not_ (be eid) <> EnemyCanBeDamagedBySource (toSource attrs)
      for_ enemies \eid' -> do
        nonAttackEnemyDamage (Just iid) attrs 1 eid'
        roundModifiers attrs eid' [CannotAttack]
      pure e
    _ -> BendBlood5 <$> liftRunMessage msg attrs
