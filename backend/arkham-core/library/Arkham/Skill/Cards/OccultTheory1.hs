module Arkham.Skill.Cards.OccultTheory1 (
  occultTheory1,
  OccultTheory1 (..),
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

newtype OccultTheory1 = OccultTheory1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

occultTheory1 :: SkillCard OccultTheory1
occultTheory1 = skill OccultTheory1 Cards.occultTheory1

instance HasModifiersFor OccultTheory1 where
  getModifiersFor (CardIdTarget cid) (OccultTheory1 a) | toCardId a == cid = do
    willpower <- field InvestigatorWillpower (skillOwner a)
    intellect <- field InvestigatorIntellect (skillOwner a)
    pure
      $ toModifiers a [AddSkillIcons $ replicate intellect #willpower <> replicate willpower #intellect]
  getModifiersFor target (OccultTheory1 a) | a `is` target = do
    willpower <- field InvestigatorWillpower (skillOwner a)
    intellect <- field InvestigatorIntellect (skillOwner a)
    pure
      $ toModifiers a [AddSkillIcons $ replicate intellect #willpower <> replicate willpower #intellect]
  getModifiersFor _ _ = pure []

instance RunMessage OccultTheory1 where
  runMessage msg (OccultTheory1 attrs) = OccultTheory1 <$> runMessage msg attrs
