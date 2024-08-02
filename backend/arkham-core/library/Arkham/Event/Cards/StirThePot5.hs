module Arkham.Event.Cards.StirThePot5 (stirThePot5, StirThePot5 (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection

newtype StirThePot5 = StirThePot5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirThePot5 :: EventCard StirThePot5
stirThePot5 = event StirThePot5 Cards.stirThePot5

instance RunMessage StirThePot5 where
  runMessage msg e@(StirThePot5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      parley sid iid attrs eid #intellect
        $ SumCalculation
          [EnemyFieldCalculation eid EnemyHealthDamage, EnemyFieldCalculation eid EnemySanityDamage]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          x <- liftA2 (+) (field EnemyHealthDamage eid) (field EnemySanityDamage eid)
          enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
          chooseOrRunOneAtATime
            iid
            [targetLabel enemy [Msg.nonAttackEnemyDamage attrs x enemy] | enemy <- enemies]
          doStep 1 msg
        _ -> error "invalid target"
      pure e
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      engaged <- select $ enemyEngagedWith iid
      canDisengage <- iid <=~> InvestigatorCanDisengage
      locations <- getAccessibleLocations iid attrs

      pushAll [DisengageEnemy iid eid | canDisengage, eid <- engaged]

      when (notNull locations) do
        chooseOne iid $ targetLabels locations (only . Move . move attrs iid)
      pure e
    _ -> StirThePot5 <$> liftRunMessage msg attrs
