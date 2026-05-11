module Arkham.Skill.Cards.Overpower (overpower) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Overpower = Overpower SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower :: SkillCard Overpower
overpower = skill Overpower Cards.overpower

instance RunMessage Overpower where
  runMessage msg s@(Overpower attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      let drawer = if attrs.cardCode.isChapterTwo then iid else attrs.owner
      skillTestCardOption attrs do
        drawCards drawer attrs 1
      pure s
    _ -> Overpower <$> liftRunMessage msg attrs
