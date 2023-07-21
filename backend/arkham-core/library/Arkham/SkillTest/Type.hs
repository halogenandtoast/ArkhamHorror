{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Type where

import Arkham.Prelude

import Arkham.SkillType
import Data.Aeson.TH

data SkillTestType = SkillSkillTest SkillType | ResourceSkillTest
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''SkillTestType)
