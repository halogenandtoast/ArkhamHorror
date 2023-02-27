module Arkham.SkillType where

import Arkham.Prelude
import GHC.OverloadedLabels

newtype CommittedSkillIcon = CommittedSkillIcon { unCommittedSkillIcon :: SkillIcon }
  deriving newtype (Show, Eq, Generic, Ord, ToJSON, FromJSON, Hashable)

data SkillType
  = SkillWillpower
  | SkillIntellect
  | SkillCombat
  | SkillAgility
  deriving stock (Show, Eq, Bounded, Enum, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, Hashable)

allSkills :: [SkillType]
allSkills = [minBound ..]

instance IsLabel "willpower" SkillType where
  fromLabel = SkillWillpower

instance IsLabel "intellect" SkillType where
  fromLabel = SkillIntellect

instance IsLabel "combat" SkillType where
  fromLabel = SkillCombat

instance IsLabel "agility" SkillType where
  fromLabel = SkillAgility

data SkillIcon = SkillIcon SkillType | WildIcon
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, Hashable)

class HasSkills a where
  toSkills :: a -> [SkillIcon]

instance IsLabel "willpower" SkillIcon where
  fromLabel = SkillIcon SkillWillpower

instance IsLabel "intellect" SkillIcon where
  fromLabel = SkillIcon SkillIntellect

instance IsLabel "combat" SkillIcon where
  fromLabel = SkillIcon SkillCombat

instance IsLabel "agility" SkillIcon where
  fromLabel = SkillIcon SkillAgility

instance IsLabel "wild" SkillIcon where
  fromLabel = WildIcon
