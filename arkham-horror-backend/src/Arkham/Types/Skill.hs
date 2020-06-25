module Arkham.Types.Skill where

import ClassyPrelude
import Data.Aeson

data ArkhamSkillType = ArkhamSkillWillpower | ArkhamSkillIntellect | ArkhamSkillCombat | ArkhamSkillAgility | ArkhamSkillWild
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype ArkhamSkill (a :: ArkhamSkillType) = ArkhamSkill { unArkhamSkill :: Int }
  deriving newtype (ToJSON, FromJSON, Show)

instance Eq ArkhamSkillType where
  ArkhamSkillWillpower == ArkhamSkillWillpower = True
  ArkhamSkillIntellect == ArkhamSkillIntellect = True
  ArkhamSkillCombat == ArkhamSkillCombat = True
  ArkhamSkillAgility == ArkhamSkillAgility = True
  ArkhamSkillWild == _ = True
  _ == ArkhamSkillWild = True
  _ == _ = False
