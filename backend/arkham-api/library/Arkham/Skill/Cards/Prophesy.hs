module Arkham.Skill.Cards.Prophesy (prophesy) where

import Arkham.Helpers.Doom
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Prophesy = Prophesy SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prophesy :: SkillCard Prophesy
prophesy = skill Prophesy Cards.prophesy

instance HasModifiersFor Prophesy where
  getModifiersFor (Prophesy attrs) = do
    doom <- getDoomCount
    addSkillIconsWhen attrs (doom >= 3) (#wild : [#wild | doom >= 6])

instance RunMessage Prophesy where
  runMessage msg (Prophesy attrs) = Prophesy <$> runMessage msg attrs
