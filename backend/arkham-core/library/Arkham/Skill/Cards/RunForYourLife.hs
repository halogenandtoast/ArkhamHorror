module Arkham.Skill.Cards.RunForYourLife
  ( runForYourLife
  , RunForYourLife(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Skill.Runner

newtype RunForYourLife = RunForYourLife SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runForYourLife :: SkillCard RunForYourLife
runForYourLife = skill RunForYourLife Cards.runForYourLife

instance RunMessage RunForYourLife where
  runMessage msg (RunForYourLife attrs) =
    RunForYourLife <$> runMessage msg attrs
