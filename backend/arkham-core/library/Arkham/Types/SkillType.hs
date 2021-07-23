module Arkham.Types.SkillType where

import Arkham.Prelude

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
