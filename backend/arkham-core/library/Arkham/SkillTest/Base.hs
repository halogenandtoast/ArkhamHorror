{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Base where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Id
import Arkham.Json
import Arkham.SkillTest.Type
import Arkham.SkillTestResult
import Arkham.SkillType (SkillType)
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

data SkillTestBaseValue
  = SkillBaseValue SkillType
  | AndSkillBaseValue [SkillType]
  | HalfResourcesOf InvestigatorId
  | StaticBaseValue Int
  deriving stock (Show, Eq, Ord)

data SkillTest = SkillTest
  { skillTestInvestigator :: InvestigatorId
  , skillTestType :: SkillTestType
  , skillTestBaseValue :: SkillTestBaseValue
  , skillTestDifficulty :: Int
  , skillTestSetAsideChaosTokens :: [ChaosToken]
  , skillTestRevealedChaosTokens :: [ChaosToken] -- tokens may change from physical representation
  , skillTestResolvedChaosTokens :: [ChaosToken]
  , skillTestValueModifier :: Int
  , skillTestResult :: SkillTestResult
  , skillTestCommittedCards :: Map InvestigatorId [Card]
  , skillTestSource :: Source
  , skillTestTarget :: Target
  , skillTestAction :: Maybe Action
  , skillTestSubscribers :: [Target]
  , skillTestIsRevelation :: Bool
  }
  deriving stock (Show, Eq, Ord)

allSkillTestChaosTokens :: SkillTest -> [ChaosToken]
allSkillTestChaosTokens SkillTest {..} =
  skillTestSetAsideChaosTokens <> skillTestRevealedChaosTokens <> skillTestResolvedChaosTokens

instance Targetable SkillTest where
  toTarget _ = SkillTestTarget
  isTarget _ SkillTestTarget = True
  isTarget _ _ = False

instance Sourceable SkillTest where
  toSource _ = SkillTestSource
  isSource _ SkillTestSource = True
  isSource _ _ = False

data SkillTestResultsData = SkillTestResultsData
  { skillTestResultsSkillValue :: Int
  , skillTestResultsIconValue :: Int
  , skillTestResultsChaosTokensValue :: Int
  , skillTestResultsDifficulty :: Int
  , skillTestResultsResultModifiers :: Maybe Int
  , skillTestResultsSuccess :: Bool
  }
  deriving stock (Eq, Show, Ord)

initSkillTest
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> Int
  -> SkillTest
initSkillTest iid source target skillType =
  buildSkillTest
    iid
    source
    target
    (SkillSkillTest skillType)
    (SkillBaseValue skillType)

buildSkillTest
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillTestType
  -> SkillTestBaseValue
  -> Int
  -> SkillTest
buildSkillTest iid (toSource -> source) (toTarget -> target) stType bValue difficulty =
  SkillTest
    { skillTestInvestigator = iid
    , skillTestType = stType
    , skillTestBaseValue = bValue
    , skillTestDifficulty = difficulty
    , skillTestSetAsideChaosTokens = mempty
    , skillTestRevealedChaosTokens = mempty
    , skillTestResolvedChaosTokens = mempty
    , skillTestValueModifier = 0
    , skillTestResult = Unrun
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestTarget = target
    , skillTestAction = Nothing
    , skillTestSubscribers = [toTarget iid]
    , skillTestIsRevelation = False
    }

$(deriveJSON defaultOptions ''SkillTestBaseValue)
$(deriveJSON defaultOptions ''SkillTestResultsData)
$(deriveJSON (aesonOptions $ Just "skillTest") ''SkillTest)
