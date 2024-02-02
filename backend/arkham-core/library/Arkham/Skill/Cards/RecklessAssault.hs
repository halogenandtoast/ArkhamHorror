module Arkham.Skill.Cards.RecklessAssault (
  recklessAssault,
  RecklessAssault (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype RecklessAssault = RecklessAssault SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

recklessAssault :: SkillCard RecklessAssault
recklessAssault = skill RecklessAssault Cards.recklessAssault

instance RunMessage RecklessAssault where
  runMessage msg (RecklessAssault attrs) =
    RecklessAssault <$> runMessage msg attrs
