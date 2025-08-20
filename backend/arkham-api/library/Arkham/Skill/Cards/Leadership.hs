module Arkham.Skill.Cards.Leadership (leadership) where

import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Leadership = Leadership SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership :: SkillCard Leadership
leadership = skill Leadership Cards.leadership

instance HasModifiersFor Leadership where
  getModifiersFor (Leadership attrs) = do
    withSkillTestInvestigator \iid -> do
      addSkillIconsWhen attrs (attrs.owner /= iid) [#willpower, #wild]

instance RunMessage Leadership where
  runMessage msg (Leadership attrs) = Leadership <$> runMessage msg attrs
