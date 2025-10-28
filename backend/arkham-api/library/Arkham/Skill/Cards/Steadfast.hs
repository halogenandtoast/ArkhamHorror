module Arkham.Skill.Cards.Steadfast (steadfast) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Steadfast = Steadfast SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

steadfast :: SkillCard Steadfast
steadfast = skill Steadfast Cards.steadfast

instance HasModifiersFor Steadfast where
  getModifiersFor (Steadfast attrs) = do
    remainingHealth <- field InvestigatorRemainingHealth attrs.owner
    remainingSanity <- field InvestigatorRemainingSanity attrs.owner
    let total = remainingHealth + remainingSanity
    addSkillIconsWhen attrs (total >= 5)
      $ if total >= 10
        then [#willpower, #willpower, #combat, #combat]
        else [#willpower, #combat]

instance RunMessage Steadfast where
  runMessage msg (Steadfast attrs) = Steadfast <$> runMessage msg attrs
