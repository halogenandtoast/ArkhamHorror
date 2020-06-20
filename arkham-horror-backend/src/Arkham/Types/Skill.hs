module Arkham.Types.Skill where

import ClassyPrelude
import Data.Aeson

data ArkhamSkillType = ArkhamSkillWillpower | ArkhamSkillIntellect | ArkhamSkillCombat | ArkhamSkillAgility
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype ArkhamSkill (a :: ArkhamSkillType) = ArkhamSkill { unArkhamSkill :: Int }
  deriving newtype (ToJSON)

