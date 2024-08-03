{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Type where

import Arkham.Prelude

import Arkham.SkillType
import Data.Aeson.TH

data SkillTestType
  = SkillSkillTest SkillType
  | AndSkillTest [SkillType]
  | ResourceSkillTest
  | BaseValueSkillTest Int [(SkillIcon, Int)]
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''SkillTestType)
