{-# LANGUAGE TemplateHaskell #-}

module Arkham.Fight.Types where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data ChooseFight = ChooseFight
  { chooseFightInvestigator :: InvestigatorId
  , chooseFightEnemyMatcher :: EnemyMatcher
  , chooseFightSource :: Source
  , chooseFightTarget :: Maybe Target
  , chooseFightSkillType :: SkillType
  , chooseFightIsAction :: Bool
  , chooseFightOnlyChoose :: Bool
  , chooseFightOverride :: Bool
  , chooseFightSkillTest :: SkillTestId
  }
  deriving stock (Show, Eq, Data)

instance HasField "investigator" ChooseFight InvestigatorId where
  getField = chooseFightInvestigator

instance HasField "isAction" ChooseFight Bool where
  getField = chooseFightIsAction

instance HasField "skillType" ChooseFight SkillType where
  getField = chooseFightSkillType

instance HasField "source" ChooseFight Source where
  getField = chooseFightSource

instance HasField "target" ChooseFight (Maybe Target) where
  getField = chooseFightTarget

instance HasField "matcher" ChooseFight EnemyMatcher where
  getField = chooseFightEnemyMatcher

instance HasField "overriden" ChooseFight Bool where
  getField = chooseFightOverride

instance HasField "onlyChoose" ChooseFight Bool where
  getField = chooseFightOnlyChoose

instance HasField "skillTest" ChooseFight SkillTestId where
  getField = chooseFightSkillTest

instance WithTarget ChooseFight where
  getTarget = chooseFightTarget
  setTarget t i = i {chooseFightTarget = Just (toTarget t)}

$(deriveJSON defaultOptions ''ChooseFight)
