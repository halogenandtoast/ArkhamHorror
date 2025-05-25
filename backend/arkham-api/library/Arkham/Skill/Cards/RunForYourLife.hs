module Arkham.Skill.Cards.RunForYourLife (runForYourLife) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype RunForYourLife = RunForYourLife SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runForYourLife :: SkillCard RunForYourLife
runForYourLife = skill RunForYourLife Cards.runForYourLife

instance RunMessage RunForYourLife where
  runMessage msg (RunForYourLife attrs) = RunForYourLife <$> runMessage msg attrs
