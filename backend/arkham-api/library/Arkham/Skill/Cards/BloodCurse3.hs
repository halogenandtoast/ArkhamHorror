module Arkham.Skill.Cards.BloodCurse3 (bloodCurse3) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype BloodCurse3 = BloodCurse3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodCurse3 :: SkillCard BloodCurse3
bloodCurse3 = skill BloodCurse3 Cards.bloodCurse3

instance RunMessage BloodCurse3 where
  runMessage msg s@(BloodCurse3 attrs) = runQueueT $ case msg of
    PassedSkillTest _iid _ _ (isTarget attrs -> True) _ _ -> do
      priority $ skillTestCardOption attrs $ do_ msg
      pure s
    Do (PassedSkillTest _iid _ _ (isTarget attrs -> True) _ _) -> do
      let controller = attrs.owner
      enemies <- select $ enemyAtLocationWith controller <> EnemyCanBeDamagedBySource (toSource attrs)
      assets <- select $ assetAtLocationWith controller <> AssetWithHealth
      investigators <- select $ colocatedWith controller
      chooseOneM controller do
        targets enemies $ nonAttackEnemyDamage (Just controller) attrs 1
        targets assets \aid -> dealAssetDamage aid attrs 1
        targets investigators \iid' -> assignDamage iid' attrs 1
      pure s
    _ -> BloodCurse3 <$> liftRunMessage msg attrs
