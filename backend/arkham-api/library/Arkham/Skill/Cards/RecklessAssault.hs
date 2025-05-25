module Arkham.Skill.Cards.RecklessAssault (recklessAssault) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype RecklessAssault = RecklessAssault SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recklessAssault :: SkillCard RecklessAssault
recklessAssault = skill RecklessAssault Cards.recklessAssault

instance RunMessage RecklessAssault where
  runMessage msg (RecklessAssault attrs) = RecklessAssault <$> runMessage msg attrs
