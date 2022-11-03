module Arkham.SkillType where

import Arkham.Prelude
import GHC.OverloadedLabels

newtype CommittedSkillIcon = CommittedSkillIcon { unCommittedSkillIcon :: SkillType }
  deriving newtype (Show, Eq, Generic, Ord, ToJSON, FromJSON, Hashable)

data SkillType
  = SkillWillpower
  | SkillIntellect
  | SkillCombat
  | SkillAgility
  | SkillWild
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, Hashable)

allSkills :: [SkillType]
allSkills = [SkillWillpower, SkillIntellect, SkillCombat, SkillAgility]

instance IsLabel "willpower" SkillType where
  fromLabel = SkillWillpower

instance IsLabel "intellect" SkillType where
  fromLabel = SkillIntellect

instance IsLabel "combat" SkillType where
  fromLabel = SkillCombat

instance IsLabel "agility" SkillType where
  fromLabel = SkillAgility

instance IsLabel "wild" SkillType where
  fromLabel = SkillWild
