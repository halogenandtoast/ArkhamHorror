{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Type where

import Arkham.Prelude

import Arkham.SkillType
import Data.Aeson.TH

data SkillTestType
  = SkillSkillTest SkillType
  | AndSkillTest [SkillType]
  | ResourceSkillTest
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, NFData)

$(deriveJSON defaultOptions ''SkillTestType)
