module Arkham.Types.Skill.Cards.RunForYourLife
  ( runForYourLife
  , RunForYourLife(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype RunForYourLife = RunForYourLife SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runForYourLife :: SkillCard RunForYourLife
runForYourLife = skill RunForYourLife Cards.runForYourLife

instance SkillRunner env => RunMessage env RunForYourLife where
  runMessage msg (RunForYourLife attrs) =
    RunForYourLife <$> runMessage msg attrs
