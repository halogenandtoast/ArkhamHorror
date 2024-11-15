module Arkham.Enemy.Cards.SpiderOfLeng (spiderOfLeng, SpiderOfLeng (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SpiderOfLeng = SpiderOfLeng EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiderOfLeng :: EnemyCard SpiderOfLeng
spiderOfLeng = enemy SpiderOfLeng Cards.spiderOfLeng (3, Static 4, 3) (1, 1)

instance HasAbilities SpiderOfLeng where
  getAbilities (SpiderOfLeng x) = extend1 x $ mkAbility x 1 $ forced $ PhaseEnds #when #enemy

instance RunMessage SpiderOfLeng where
  runMessage msg e@(SpiderOfLeng attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      lead <- getLead
      swarmsOfSpiders <- select $ enemyIs Cards.swarmOfSpiders <> not_ IsSwarm
      if null swarmsOfSpiders
        then findEncounterCard lead attrs Cards.swarmOfSpiders
        else chooseOneAtATimeM lead $ targets swarmsOfSpiders \eid -> push $ PlaceSwarmCards lead eid 1
      pure e
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      this <- createEnemy card (locationWithEnemy attrs.id)
      abilityModifier (AbilityRef (toSource attrs) 1) (attrs.ability 1) this NoInitialSwarm
      pure e
    _ -> SpiderOfLeng <$> liftRunMessage msg attrs
