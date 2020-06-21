module Arkham.Types.Skill where

import ClassyPrelude
import Data.Aeson

data ArkhamSkillType = ArkhamSkillWillpower | ArkhamSkillIntellect | ArkhamSkillCombat | ArkhamSkillAgility
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype ArkhamSkill (a :: ArkhamSkillType) = ArkhamSkill { unArkhamSkill :: Int }
  deriving newtype (ToJSON, FromJSON, Show)
