module Arkham.SkillTest.Base where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Classes.Entity
import Arkham.Card
import Arkham.Id
import Arkham.Json
import Arkham.SkillTestResult
import Arkham.SkillTest.Type
import Arkham.SkillType (SkillType)
import Arkham.Source
import Arkham.Target
import Arkham.Token

data SkillTestBaseValue
  = SkillBaseValue SkillType
  | HalfResourcesOf InvestigatorId
  | StaticBaseValue Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

data SkillTest = SkillTest
  { skillTestInvestigator :: InvestigatorId
  , skillTestType :: SkillTestType
  , skillTestBaseValue :: SkillTestBaseValue
  , skillTestDifficulty :: Int
  , skillTestSetAsideTokens :: [Token]
  , skillTestRevealedTokens :: [Token] -- tokens may change from physical representation
  , skillTestResolvedTokens :: [Token]
  , skillTestValueModifier :: Int
  , skillTestResult :: SkillTestResult
  , skillTestCommittedCards :: HashMap InvestigatorId [Card]
  , skillTestSource :: Source
  , skillTestTarget :: Target
  , skillTestAction :: Maybe Action
  , skillTestSubscribers :: [Target]
  , skillTestIsRevelation :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

allSkillTestTokens :: SkillTest -> [Token]
allSkillTestTokens SkillTest {..} =
  skillTestSetAsideTokens
  <> skillTestRevealedTokens
  <> skillTestResolvedTokens

instance ToJSON SkillTest where
  toJSON = genericToJSON $ aesonOptions $ Just "skillTest"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skillTest"

instance FromJSON SkillTest where
  parseJSON = withObject "SkillTest" $ \o -> SkillTest
    <$> o .: "investigator"
    <*> o .: "type"
    <*> o .: "baseValue"
    <*> o .: "difficulty"
    <*> o .: "setAsideTokens"
    <*> o .: "revealedTokens"
    <*> o .: "resolvedTokens"
    <*> o .: "valueModifier"
    <*> o .: "result"
    <*> o .: "committedCards"
    <*> o .: "source"
    <*> o .: "target"
    <*> o .: "action"
    <*> o .: "subscribers"
    <*> o .:? "isRevelation" .!= False

instance Targetable SkillTest where
  toTarget _ = SkillTestTarget
  isTarget _ SkillTestTarget = True
  isTarget _ _ = False

instance SourceEntity SkillTest where
  toSource SkillTest {..} =
    SkillTestSource
      skillTestInvestigator
      skillTestType
      skillTestSource
      skillTestAction
  isSource _ SkillTestSource {} = True
  isSource _ _ = False

data SkillTestResultsData = SkillTestResultsData
  { skillTestResultsSkillValue :: Int
  , skillTestResultsIconValue :: Int
  , skillTestResultsTokensValue :: Int
  , skillTestResultsDifficulty :: Int
  , skillTestResultsResultModifiers :: Maybe Int
  , skillTestResultsSuccess :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

initSkillTest ::
  InvestigatorId ->
  Source ->
  Target ->
  SkillType ->
  Int ->
  SkillTest
initSkillTest iid source target skillType =
  buildSkillTest iid source target (SkillSkillTest skillType) (SkillBaseValue skillType)

buildSkillTest ::
  InvestigatorId ->
  Source ->
  Target ->
  SkillTestType ->
  SkillTestBaseValue ->
  Int ->
  SkillTest
buildSkillTest iid source target stType bValue difficulty =
  SkillTest
    { skillTestInvestigator = iid
    , skillTestType = stType
    , skillTestBaseValue = bValue
    , skillTestDifficulty = difficulty
    , skillTestSetAsideTokens = mempty
    , skillTestRevealedTokens = mempty
    , skillTestResolvedTokens = mempty
    , skillTestValueModifier = 0
    , skillTestResult = Unrun
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestTarget = target
    , skillTestAction = Nothing
    , skillTestSubscribers = [InvestigatorTarget iid]
    , skillTestIsRevelation = False
    }
