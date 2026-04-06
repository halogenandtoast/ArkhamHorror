module Arkham.Skill.Cards.Rough1 (rough1) where

import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (InvestigatorDamage)

newtype Rough1 = Rough1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rough1 :: SkillCard Rough1
rough1 = skill Rough1 Cards.rough1

instance HasModifiersFor Rough1 where
  getModifiersFor (Rough1 a) = maybeModified_ a a.cardId do
    amount <- lift $ fieldMap InvestigatorDamage ((`div` 2) . succ) a.owner
    guard $ amount > 0
    pure [AddSkillIcons $ replicate amount #wild]
