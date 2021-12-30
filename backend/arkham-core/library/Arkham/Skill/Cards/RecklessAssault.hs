module Arkham.Skill.Cards.RecklessAssault
  ( recklessAssault
  , RecklessAssault(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Skill.Attrs
import Arkham.Skill.Runner

newtype RecklessAssault = RecklessAssault SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recklessAssault :: SkillCard RecklessAssault
recklessAssault = skill RecklessAssault Cards.recklessAssault

instance SkillRunner env => RunMessage env RecklessAssault where
  runMessage msg (RecklessAssault attrs) =
    RecklessAssault <$> runMessage msg attrs
