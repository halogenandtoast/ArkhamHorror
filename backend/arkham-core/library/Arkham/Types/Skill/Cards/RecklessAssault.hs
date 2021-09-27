module Arkham.Types.Skill.Cards.RecklessAssault
  ( recklessAssault
  , RecklessAssault(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype RecklessAssault = RecklessAssault SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recklessAssault :: SkillCard RecklessAssault
recklessAssault = skill RecklessAssault Cards.recklessAssault

instance SkillRunner env => RunMessage env RecklessAssault where
  runMessage msg (RecklessAssault attrs) =
    RecklessAssault <$> runMessage msg attrs
