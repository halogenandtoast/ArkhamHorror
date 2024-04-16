{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Base where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Id
import Arkham.Json
import Arkham.SkillTest.Type
import Arkham.SkillTestResult
import Arkham.SkillType (SkillIcon (..), SkillType)
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data SkillTestBaseValue
  = SkillBaseValue SkillType
  | AndSkillBaseValue [SkillType]
  | HalfResourcesOf InvestigatorId
  deriving stock (Show, Eq)

newtype SkillTestDifficulty = SkillTestDifficulty GameCalculation
  deriving newtype (Show, Ord, Eq, Generic, FromJSON, ToJSON)
  deriving stock (Data)

data SkillTest = SkillTest
  { skillTestInvestigator :: InvestigatorId
  , skillTestResolveFailureInvestigator :: InvestigatorId
  , skillTestType :: SkillTestType
  , skillTestBaseValue :: SkillTestBaseValue
  , skillTestDifficulty :: SkillTestDifficulty
  , skillTestSetAsideChaosTokens :: [ChaosToken]
  , skillTestRevealedChaosTokens :: [ChaosToken] -- tokens may change from physical representation
  , skillTestResolvedChaosTokens :: [ChaosToken]
  , skillTestToResolveChaosTokens :: [ChaosToken]
  , skillTestResult :: SkillTestResult
  , skillTestCommittedCards :: Map InvestigatorId [Card]
  , skillTestSource :: Source
  , skillTestTarget :: Target
  , skillTestAction :: Maybe Action
  , skillTestSubscribers :: [Target]
  , skillTestIsRevelation :: Bool
  , skillTestIconValues :: Map SkillIcon Int
  , skillTestCard :: Maybe CardId
  }
  deriving stock (Show, Eq)

instance HasField "action" SkillTest (Maybe Action) where
  getField = skillTestAction

instance HasField "target" SkillTest Target where
  getField = skillTestTarget

instance HasField "investigator" SkillTest InvestigatorId where
  getField = skillTestInvestigator

instance HasField "revealedChaosTokens" SkillTest [ChaosToken] where
  getField = skillTestRevealedChaosTokens

instance HasField "chaosTokens" SkillTest [ChaosToken] where
  getField = skillTestSetAsideChaosTokens

allSkillTestChaosTokens :: SkillTest -> [ChaosToken]
allSkillTestChaosTokens SkillTest {..} = skillTestSetAsideChaosTokens

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
  deriving stock (Eq, Show)

initSkillTest
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> SkillTestDifficulty
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
  -> SkillTestDifficulty
  -> SkillTest
buildSkillTest iid (toSource -> source) (toTarget -> target) stType bValue difficulty =
  SkillTest
    { skillTestInvestigator = iid
    , skillTestResolveFailureInvestigator = iid
    , skillTestType = stType
    , skillTestBaseValue = bValue
    , skillTestDifficulty = difficulty
    , skillTestSetAsideChaosTokens = mempty
    , skillTestRevealedChaosTokens = mempty
    , skillTestResolvedChaosTokens = mempty
    , skillTestToResolveChaosTokens = mempty
    , skillTestResult = Unrun
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestTarget = target
    , skillTestAction = Nothing
    , skillTestSubscribers = [toTarget iid]
    , skillTestIsRevelation = False
    , skillTestIconValues = iconValuesForSkillTestType stType
    , skillTestCard = Nothing
    }

iconValuesForSkillTestType :: SkillTestType -> Map SkillIcon Int
iconValuesForSkillTestType = \case
  SkillSkillTest skillType -> base <> singletonMap (SkillIcon skillType) 1
  AndSkillTest skillTypes -> base <> mapFromList (map ((,1) . SkillIcon) skillTypes)
  ResourceSkillTest -> base
 where
  base = mapFromList [(#wild, 1), (#wildMinus, -1)]

resetSkillTest :: SkillTest -> SkillTest
resetSkillTest skillTest =
  skillTest
    { skillTestSetAsideChaosTokens = mempty
    , skillTestRevealedChaosTokens = mempty
    , skillTestResolvedChaosTokens = mempty
    , skillTestToResolveChaosTokens = mempty
    , skillTestResult = Unrun
    , skillTestCommittedCards = mempty
    , skillTestSubscribers = [toTarget $ skillTestInvestigator skillTest]
    }

$(deriveJSON defaultOptions ''SkillTestBaseValue)
$(deriveJSON defaultOptions ''SkillTestResultsData)

instance FromJSON SkillTest where
  parseJSON = withObject "skillTest" $ \o -> do
    skillTestInvestigator <- o .: "investigator"
    skillTestResolveFailureInvestigator <- o .: "resolveFailureInvestigator"
    skillTestType <- o .: "type"
    skillTestBaseValue <- o .: "baseValue"
    skillTestDifficulty <- o .: "difficulty"
    skillTestSetAsideChaosTokens <- o .: "setAsideChaosTokens"
    skillTestRevealedChaosTokens <- o .: "revealedChaosTokens"
    skillTestResolvedChaosTokens <- o .: "resolvedChaosTokens"
    skillTestToResolveChaosTokens <- o .:? "toResolveChaosTokens" .!= []
    skillTestResult <- o .: "result"
    skillTestCommittedCards <- o .: "committedCards"
    skillTestSource <- o .: "source"
    skillTestTarget <- o .: "target"
    skillTestAction <- o .: "action"
    skillTestSubscribers <- o .: "subscribers"
    skillTestIsRevelation <- o .: "isRevelation"
    skillTestIconValues <- o .:? "iconValues" .!= iconValuesForSkillTestType skillTestType
    skillTestCard <- o .:? "card"
    pure SkillTest {..}

$(deriveToJSON (aesonOptions $ Just "skillTest") ''SkillTest)
