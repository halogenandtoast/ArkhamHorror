module Arkham.Event.Cards.StirThePot (stirThePot, StirThePot (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Movement

newtype StirThePot = StirThePot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirThePot :: EventCard StirThePot
stirThePot = event StirThePot Cards.stirThePot

instance RunMessage StirThePot where
  runMessage msg e@(StirThePot attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      parley sid iid attrs eid #intellect
        $ SumCalculation
          [EnemyFieldCalculation eid EnemyHealthDamage, EnemyFieldCalculation eid EnemySanityDamage]
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseOrRunOneAtATime
        iid
        [targetLabel enemy [Msg.nonAttackEnemyDamage attrs 2 enemy] | enemy <- enemies]
      when (n >= 2) $ doStep 1 msg
      pure e
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      engaged <- select $ enemyEngagedWith iid
      canDisengage <- iid <=~> InvestigatorCanDisengage
      locations <- getAccessibleLocations iid attrs

      pushAll [DisengageEnemy iid eid | canDisengage, eid <- engaged]

      when (notNull locations) do
        chooseOne iid $ targetLabels locations (only . Move . move attrs iid)
      pure e
    _ -> StirThePot <$> liftRunMessage msg attrs
