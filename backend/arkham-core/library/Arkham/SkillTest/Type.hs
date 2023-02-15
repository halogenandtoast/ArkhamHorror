module Arkham.SkillTest.Type where

import Arkham.Prelude

import Arkham.SkillType

data SkillTestType = SkillSkillTest SkillType | ResourceSkillTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)
