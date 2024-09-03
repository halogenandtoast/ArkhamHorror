module Arkham.Skill.Cards.JustifyTheMeans3 (justifyTheMeans3, JustifyTheMeans3 (..)) where

import Arkham.Classes
import Arkham.Cost
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype JustifyTheMeans3 = JustifyTheMeans3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justifyTheMeans3 :: SkillCard JustifyTheMeans3
justifyTheMeans3 =
  skillWith
    JustifyTheMeans3
    Cards.justifyTheMeans3
    (additionalCostL ?~ AddCurseTokensEqualToSkillTestDifficulty)

instance RunMessage JustifyTheMeans3 where
  runMessage msg (JustifyTheMeans3 attrs) = case msg of
    InvestigatorCommittedSkill _iid sid | sid == toId attrs -> do
      push PassSkillTest
      JustifyTheMeans3 <$> runMessage msg attrs
    _ -> JustifyTheMeans3 <$> runMessage msg attrs
