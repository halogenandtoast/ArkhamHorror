module Arkham.Skill.Cards.JustifyTheMeans3 (justifyTheMeans3) where

import Arkham.Cost
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype JustifyTheMeans3 = JustifyTheMeans3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justifyTheMeans3 :: SkillCard JustifyTheMeans3
justifyTheMeans3 =
  skillWith JustifyTheMeans3 Cards.justifyTheMeans3
    $ additionalCostL
    ?~ AddCurseTokensEqualToSkillTestDifficulty

instance RunMessage JustifyTheMeans3 where
  runMessage msg (JustifyTheMeans3 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill _iid sid | sid == toId attrs -> do
      passSkillTest
      JustifyTheMeans3 <$> liftRunMessage msg attrs
    _ -> JustifyTheMeans3 <$> liftRunMessage msg attrs
