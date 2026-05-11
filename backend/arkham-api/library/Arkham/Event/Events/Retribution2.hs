module Arkham.Event.Events.Retribution2 (retribution2) where

import Arkham.Attack
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Projection
import Arkham.Strategy (AfterPlayStrategy (DeferDiscard))

newtype Retribution2 = Retribution2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

retribution2 :: EventCard Retribution2
retribution2 = event Retribution2 Cards.retribution2

instance RunMessage Retribution2 where
  runMessage msg e@(Retribution2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let attack = getAttackDetails attrs.windows
      damage <- field EnemyHealthDamage attack.enemy
      horror <- field EnemySanityDamage attack.enemy
      let n = damage + horror
      changeAttackDetails attack.enemy
        $ attack
          { attackTarget = SingleAttackTarget (toTarget iid)
          , attackAfter = attackAfter attack <> [DoStep n msg]
          }
      pure $ Retribution2 (attrs & afterPlayL .~ DeferDiscard)
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      if n > 0 && not (null enemies)
        then chooseOneM iid do
          labeledI "done" $ finalizeEvent attrs.id
          targets enemies \eid -> do
            nonAttackEnemyDamage (Just iid) attrs 1 eid
            doStep (n - 1) msg'
        else finalizeEvent attrs.id
      pure e
    _ -> Retribution2 <$> liftRunMessage msg attrs
