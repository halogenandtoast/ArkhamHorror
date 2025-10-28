module Arkham.Skill.Cards.Curiosity (curiosity) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Curiosity = Curiosity SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curiosity :: SkillCard Curiosity
curiosity = skill Curiosity Cards.curiosity

instance HasModifiersFor Curiosity where
  getModifiersFor (Curiosity attrs) = do
    cardsInHand <- fieldMap InvestigatorHand length attrs.owner
    addSkillIconsWhen attrs (cardsInHand >= 4)
      $ if cardsInHand >= 7
        then [#willpower, #willpower, #intellect, #intellect]
        else [#willpower, #intellect]

instance RunMessage Curiosity where
  runMessage msg (Curiosity attrs) = Curiosity <$> runMessage msg attrs
