module Arkham.Enemy.Cards.CubicOoze (cubicOoze) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CubicOoze = CubicOoze EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cubicOoze :: EnemyCard CubicOoze
cubicOoze = enemyWith CubicOoze Cards.cubicOoze ((spawnAtL ?~ NoSpawn) . (delayEngagementL .~ True))

instance RunMessage CubicOoze where
  runMessage msg e@(CubicOoze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ TryEvadeEnemy sid iid attrs.id (IndexedSource 0 (toSource attrs)) Nothing #agility
      pure e
    PassedThisSkillTest iid (IndexedSource 0 (isSource attrs -> True)) -> do
      push
        $ EnemySpawn
        $ (mkSpawnDetails attrs.id $ SpawnAt (locationWithInvestigator iid))
          { spawnDetailsExhausted = True
          , spawnDetailsUnengaged = True
          }
      pure $ CubicOoze $ attrs & delayEngagementL .~ False & exhaustedL .~ True
    FailedThisSkillTest iid (IndexedSource 0 (isSource attrs -> True)) -> do
      push $ EnemySpawnEngagedWith (toId attrs) (InvestigatorWithId iid)
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure $ CubicOoze $ attrs & delayEngagementL .~ False
    _ -> CubicOoze <$> liftRunMessage msg attrs
