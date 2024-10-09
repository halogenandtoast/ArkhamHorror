{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.SkillType where

import Arkham.Prelude
import Arkham.SkillType
import Data.Aeson.TH
import GHC.OverloadedLabels

data SkillTypeMatcher
  = AnySkillType
  | NotSkillType SkillType
  | IsSkillType SkillType
  | SkillTypeOneOf [SkillType]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" SkillTypeMatcher where
  fromLabel = AnySkillType

$(deriveJSON defaultOptions ''SkillTypeMatcher)
