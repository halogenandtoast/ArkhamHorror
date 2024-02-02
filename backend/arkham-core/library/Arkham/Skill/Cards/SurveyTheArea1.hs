module Arkham.Skill.Cards.SurveyTheArea1 (
  surveyTheArea1,
  SurveyTheArea1 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SurveyTheArea1 = SurveyTheArea1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

surveyTheArea1 :: SkillCard SurveyTheArea1
surveyTheArea1 = skill SurveyTheArea1 Cards.surveyTheArea1

instance HasModifiersFor SurveyTheArea1 where
  getModifiersFor (CardIdTarget cid) (SurveyTheArea1 a) | toCardId a == cid = do
    agility <- field InvestigatorAgility (skillOwner a)
    intellect <- field InvestigatorIntellect (skillOwner a)
    pure $ toModifiers a [AddSkillIcons $ replicate intellect #agility <> replicate agility #intellect]
  getModifiersFor target (SurveyTheArea1 a) | a `is` target = do
    agility <- field InvestigatorAgility (skillOwner a)
    intellect <- field InvestigatorIntellect (skillOwner a)
    pure $ toModifiers a [AddSkillIcons $ replicate intellect #agility <> replicate agility #intellect]
  getModifiersFor _ _ = pure []

instance RunMessage SurveyTheArea1 where
  runMessage msg (SurveyTheArea1 attrs) = SurveyTheArea1 <$> runMessage msg attrs
