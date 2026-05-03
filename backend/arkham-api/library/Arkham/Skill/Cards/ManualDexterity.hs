module Arkham.Skill.Cards.ManualDexterity (manualDexterity) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype ManualDexterity = ManualDexterity SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manualDexterity :: SkillCard ManualDexterity
manualDexterity = skill ManualDexterity Cards.manualDexterity

instance RunMessage ManualDexterity where
  runMessage msg s@(ManualDexterity attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      let drawer = if attrs.cardCode.isChapterTwo then iid else attrs.owner
      additionalSkillTestOption "Manual Dexterity" do
        drawCards drawer attrs 1
      pure s
    _ -> ManualDexterity <$> liftRunMessage msg attrs
