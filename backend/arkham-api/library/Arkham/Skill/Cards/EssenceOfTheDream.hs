module Arkham.Skill.Cards.EssenceOfTheDream (
  essenceOfTheDream,
  EssenceOfTheDream (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

-- N.B.: This card is hard coded in Investigator/Runner as the ability is
-- global no matter where the card is. This makes it hard to make sure the
-- effect would be active (for example the skill is already removed when adding
-- to the discard). We could encode this on the card, but it's very bespoke.

newtype EssenceOfTheDream = EssenceOfTheDream SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

essenceOfTheDream :: SkillCard EssenceOfTheDream
essenceOfTheDream = skill EssenceOfTheDream Cards.essenceOfTheDream

instance RunMessage EssenceOfTheDream where
  runMessage msg (EssenceOfTheDream attrs) = EssenceOfTheDream <$> runMessage msg attrs
