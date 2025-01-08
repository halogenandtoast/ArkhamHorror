module Arkham.Skill.Cards.OccultTheory1 (occultTheory1, OccultTheory1 (..)) where

import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype OccultTheory1 = OccultTheory1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultTheory1 :: SkillCard OccultTheory1
occultTheory1 = skill OccultTheory1 Cards.occultTheory1

instance HasModifiersFor OccultTheory1 where
  getModifiersFor (OccultTheory1 a) = do
    willpower <- field InvestigatorWillpower a.owner
    intellect <- field InvestigatorIntellect a.owner
    modifySelf
      a.cardId
      [AddSkillIcons $ replicate intellect #willpower <> replicate willpower #intellect]

instance RunMessage OccultTheory1 where
  runMessage msg (OccultTheory1 attrs) = OccultTheory1 <$> runMessage msg attrs
