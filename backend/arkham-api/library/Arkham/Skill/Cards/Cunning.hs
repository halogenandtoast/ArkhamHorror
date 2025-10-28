module Arkham.Skill.Cards.Cunning (cunning) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Cunning = Cunning SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunning :: SkillCard Cunning
cunning = skill Cunning Cards.cunning

instance HasModifiersFor Cunning where
  getModifiersFor (Cunning attrs) = do
    resources <- field InvestigatorResources attrs.owner
    addSkillIconsWhen attrs (resources >= 5)
      $ if resources >= 10
        then [#intellect, #intellect, #agility, #agility]
        else [#intellect, #agility]

instance RunMessage Cunning where
  runMessage msg (Cunning attrs) = Cunning <$> runMessage msg attrs
