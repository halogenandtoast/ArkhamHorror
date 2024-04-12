module Arkham.SkillTest.Base where

import Arkham.Prelude

data SkillTest

instance Eq SkillTest
instance Show SkillTest
instance ToJSON SkillTest
instance FromJSON SkillTest

data SkillTestBaseValue

instance Eq SkillTestBaseValue
instance Show SkillTestBaseValue
instance ToJSON SkillTestBaseValue
instance FromJSON SkillTestBaseValue

data SkillTestDifficulty

instance Eq SkillTestDifficulty
instance Show SkillTestDifficulty
instance ToJSON SkillTestDifficulty
instance FromJSON SkillTestDifficulty
instance Data SkillTestDifficulty
instance Ord SkillTestDifficulty

data SkillTestResultsData

instance Eq SkillTestResultsData
instance Show SkillTestResultsData
instance ToJSON SkillTestResultsData
instance FromJSON SkillTestResultsData
