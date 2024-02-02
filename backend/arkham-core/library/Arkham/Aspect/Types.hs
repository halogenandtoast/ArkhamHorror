module Arkham.Aspect.Types where

import Arkham.Prelude

import Arkham.SkillType

data Aspect = InsteadOfAspect InsteadOf
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

data InsteadOf = InsteadOf SkillType SkillType
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)
