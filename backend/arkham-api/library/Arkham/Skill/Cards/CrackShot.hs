module Arkham.Skill.Cards.CrackShot (crackShot) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype CrackShot = CrackShot SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crackShot :: SkillCard CrackShot
crackShot = skill CrackShot Cards.crackShot

instance RunMessage CrackShot where
  runMessage msg (CrackShot attrs) = CrackShot <$> runMessage msg attrs
