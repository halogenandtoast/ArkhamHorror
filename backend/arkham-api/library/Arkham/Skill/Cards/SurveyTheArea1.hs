module Arkham.Skill.Cards.SurveyTheArea1 (
  surveyTheArea1,
  SurveyTheArea1 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SurveyTheArea1 = SurveyTheArea1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surveyTheArea1 :: SkillCard SurveyTheArea1
surveyTheArea1 = skill SurveyTheArea1 Cards.surveyTheArea1

instance HasModifiersFor SurveyTheArea1 where
  getModifiersFor (SurveyTheArea1 a) = do
    agility <- field InvestigatorAgility a.owner
    intellect <- field InvestigatorIntellect a.owner
    modifySelf a.cardId [AddSkillIcons $ replicate intellect #agility <> replicate agility #intellect]

instance RunMessage SurveyTheArea1 where
  runMessage msg (SurveyTheArea1 attrs) = SurveyTheArea1 <$> runMessage msg attrs
