module Arkham.Skill.Cards.SoulLink (soulLink) where

import Arkham.Cost
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SoulLink = SoulLink SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soulLink :: SkillCard SoulLink
soulLink = skillWith SoulLink Cards.soulLink $ additionalCostL ?~ HorrorCost ThisCard YouTarget 1
