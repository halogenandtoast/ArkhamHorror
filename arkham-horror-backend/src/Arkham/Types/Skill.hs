module Arkham.Types.Skill where

import GHC.Generics
import Json
import Prelude (Show)

data ArkhamSkill
  = ArkhamSkillWillpower
  | ArkhamSkillCombat
  | ArkhamSkillIntellect
  | ArkhamSkillAgility
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "skill" ArkhamSkill
