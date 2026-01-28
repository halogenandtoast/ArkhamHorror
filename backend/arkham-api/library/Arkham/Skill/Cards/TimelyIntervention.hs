module Arkham.Skill.Cards.TimelyIntervention (timelyIntervention) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype TimelyIntervention = TimelyIntervention SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelyIntervention :: SkillCard TimelyIntervention
timelyIntervention = skill TimelyIntervention Cards.timelyIntervention
