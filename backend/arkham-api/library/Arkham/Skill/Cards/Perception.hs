module Arkham.Skill.Cards.Perception (perception) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Perception = Perception SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perception :: SkillCard Perception
perception = skill Perception Cards.perception

instance RunMessage Perception where
  runMessage msg s@(Perception attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      let drawer = if attrs.cardCode.isChapterTwo then iid else attrs.owner
      additionalSkillTestOption "Perception" do
        drawCards drawer attrs 1
      pure s
    _ -> Perception <$> liftRunMessage msg attrs
