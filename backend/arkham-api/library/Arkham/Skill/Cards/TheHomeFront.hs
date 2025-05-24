module Arkham.Skill.Cards.TheHomeFront (theHomeFront) where

import Arkham.Action
import Arkham.Helpers.Enemy
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (InvestigatorDamage)
import Arkham.SkillTest

newtype TheHomeFront = TheHomeFront SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHomeFront :: SkillCard TheHomeFront
theHomeFront = skill TheHomeFront Cards.theHomeFront

instance RunMessage TheHomeFront where
  runMessage msg s@(TheHomeFront attrs) = runQueueT $ case msg of
    PassedSkillTest _ (Just Fight) _ (isTarget attrs -> True) _ _ -> do
      getSkillTestTargetedEnemy >>= traverse_ \eid -> do
        damageCount <- field InvestigatorDamage attrs.owner
        canDamage <- sourceCanDamageEnemy eid (toSource attrs)
        when (canDamage && damageCount > 0) do
          push $ HealDamageDirectly (InvestigatorTarget attrs.owner) (toSource attrs) 1
          nonAttackEnemyDamage (Just attrs.owner) attrs 1 eid
      pure s
    _ -> TheHomeFront <$> liftRunMessage msg attrs
