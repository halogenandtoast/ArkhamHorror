module Arkham.Types.SkillType where

import Arkham.Prelude

data SkillType
  = SkillWillpower
  | SkillIntellect
  | SkillCombat
  | SkillAgility
  | SkillWild
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
