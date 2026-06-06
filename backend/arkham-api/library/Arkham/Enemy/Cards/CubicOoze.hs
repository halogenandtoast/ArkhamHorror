module Arkham.Enemy.Cards.CubicOoze (cubicOoze) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CubicOoze = CubicOoze EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cubicOoze :: EnemyCard CubicOoze
cubicOoze = enemyWith CubicOoze Cards.cubicOoze (1, Static 4, 4) (2, 0) (spawnAtL ?~ NoSpawn)

instance RunMessage CubicOoze where
  runMessage msg e@(CubicOoze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Spawn engaged with you, then immediately attempt to evade. A successful
      -- evade exhausts and disengages it (spawned at your location, exhausted
      -- and unengaged); a failed evade leaves it engaged and it attacks you.
      push $ EnemySpawnEngagedWith (toId attrs) (InvestigatorWithId iid)
      sid <- getRandom
      chooseEvadeEnemyMatch sid iid (attrs.ability 1) (EnemyWithId $ toId attrs)
      pure e
    FailedSkillTest iid (Just action) (isAbilitySource attrs 1 -> True) _ _ _ | action == #evade -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> CubicOoze <$> liftRunMessage msg attrs
