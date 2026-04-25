module Arkham.Enemy.Cards.SgtEarlMonroeDirtyCop (sgtEarlMonroeDirtyCop) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDamage)
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Projection

newtype SgtEarlMonroeDirtyCop = SgtEarlMonroeDirtyCop EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sgtEarlMonroeDirtyCop :: EnemyCard SgtEarlMonroeDirtyCop
sgtEarlMonroeDirtyCop =
  enemy SgtEarlMonroeDirtyCop Cards.sgtEarlMonroeDirtyCop (3, Static 4, 3) (1, 1)

instance HasAbilities SgtEarlMonroeDirtyCop where
  getAbilities (SgtEarlMonroeDirtyCop a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ ActionAbility #parley (Just $ AbilitySkill #combat) (ActionCost 1)

instance RunMessage SgtEarlMonroeDirtyCop where
  runMessage msg e@(SgtEarlMonroeDirtyCop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      damage <- field EnemyDamage attrs.id
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed $ max 0 (4 - damage))
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 4
      pure e
    _ -> SgtEarlMonroeDirtyCop <$> liftRunMessage msg attrs
