module Arkham.Skill.Cards.BloodCurse (bloodCurse) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype BloodCurse = BloodCurse SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodCurse :: SkillCard BloodCurse
bloodCurse = skill BloodCurse Cards.bloodCurse

instance RunMessage BloodCurse where
  runMessage msg s@(BloodCurse attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      additionalSkillTestOption "Blood Curse" $ directDamage iid attrs 1
      pure s
    _ -> BloodCurse <$> liftRunMessage msg attrs
