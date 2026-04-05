module Arkham.Skill.Cards.TimelyIntervention3 (timelyIntervention3) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype TimelyIntervention3 = TimelyIntervention3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelyIntervention3 :: SkillCard TimelyIntervention3
timelyIntervention3 = skill TimelyIntervention3 Cards.timelyIntervention3
