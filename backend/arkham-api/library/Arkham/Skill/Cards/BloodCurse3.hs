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
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      assets <- select $ assetAtLocationWith iid <> AssetWithHealth
      investigators <- select $ colocatedWith iid
      unless (null enemies && null assets && null investigators) do
        additionalSkillTestOption "Blood Curse" do
          chooseOneM iid do
            targets enemies $ nonAttackEnemyDamage (Just iid) attrs 1
            targets assets \aid -> dealAssetDamage aid attrs 1
            targets investigators \iid' -> assignDamage iid' attrs 1
      pure s
    _ -> BloodCurse3 <$> liftRunMessage msg attrs
