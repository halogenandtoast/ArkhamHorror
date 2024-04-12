{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Base where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Asset.Types
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Enemy.Types
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Json
import Arkham.Location.Types
import Arkham.Matcher.Types
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Type
import Arkham.SkillTestResult
import Arkham.SkillType (SkillIcon (..), SkillType)
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Data.Aeson.TH
import GHC.Records

data SkillTestBaseValue
  = SkillBaseValue SkillType
  | AndSkillBaseValue [SkillType]
  | HalfResourcesOf InvestigatorId
  | StaticBaseValue Int
  deriving stock (Show, Eq)

data SkillTestDifficulty
  = Fixed Int
  | ActsInPlayDifficulty
  | MaxDifficulty Int SkillTestDifficulty
  | CurrentAgendaStepDifficulty SkillTestDifficulty
  | DividedByDifficulty SkillTestDifficulty Int
  | RecordCountDifficulty CampaignLogKey
  | ScenarioCountDifficulty ScenarioCountKey
  | SumDifficulty [SkillTestDifficulty]
  | SubtractDifficulty SkillTestDifficulty SkillTestDifficulty
  | AssetFieldDifficulty AssetId (Field Asset Int)
  | InvestigatorFieldDifficulty InvestigatorId (Field Investigator Int)
  | InvestigatorHandLengthDifficulty InvestigatorId
  | EnemyMaybeFieldDifficulty EnemyId (Field Enemy (Maybe Int))
  | EnemyMaybeGameValueFieldDifficulty EnemyId (Field Enemy (Maybe GameValue))
  | EnemyFieldDifficulty EnemyId (Field Enemy Int)
  | EnemyCountDifficulty EnemyMatcher
  | LocationCountDifficulty LocationMatcher
  | VictoryDisplayCountDifficulty CardMatcher
  | LocationFieldDifficulty LocationId (Field Location Int)
  | InvestigatorLocationFieldDifficulty InvestigatorId (Field Location Int)
  | CardCostDifficulty CardId
  | ScenarioInDiscardCountDifficulty CardMatcher
  | DoomCountDifficulty
  | DistanceFromDifficulty InvestigatorId LocationMatcher
  | InvestigatorTokenCountDifficulty InvestigatorId Token
  | MaxAlarmLevelDifficulty -- getMaxAlarmLevel
  | VengeanceDifficulty -- getVengeanceInVictoryDisplay
  deriving stock (Show, Ord, Eq, Data, Generic)
  deriving (FromJSON) via MaybeFixed

newtype MaybeFixed = MaybeFixed SkillTestDifficulty

instance FromJSON MaybeFixed where
  parseJSON = \case
    v@(Number _) -> MaybeFixed . Fixed <$> parseJSON v
    other -> MaybeFixed <$> genericParseJSON defaultOptions other

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
$(deriveToJSON defaultOptions ''SkillTestDifficulty)

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
