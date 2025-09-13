module Arkham.Skill.Cards.StrengthInNumbers1 (strengthInNumbers1) where

import Arkham.Helpers.Calculation
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype StrengthInNumbers1 = StrengthInNumbers1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strengthInNumbers1 :: SkillCard StrengthInNumbers1
strengthInNumbers1 = skill StrengthInNumbers1 Cards.strengthInNumbers1

instance HasModifiersFor StrengthInNumbers1 where
  getModifiersFor (StrengthInNumbers1 a) = do
    n <-
      calculate
        (DifferentClassAmong (InvestigatorWithId a.owner) $ ControlledBy $ InvestigatorWithId a.owner)
    addSkillIcons a $ replicate n #wild

instance RunMessage StrengthInNumbers1 where
  runMessage msg (StrengthInNumbers1 attrs) = StrengthInNumbers1 <$> runMessage msg attrs
