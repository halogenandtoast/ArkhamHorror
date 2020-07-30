module Arkham.Types.SkillType where

import ClassyPrelude
import Data.Aeson

data SkillType
  = SkillWillpower
  | SkillIntellect
  | SkillCombat
  | SkillAgility
  | SkillWild
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
