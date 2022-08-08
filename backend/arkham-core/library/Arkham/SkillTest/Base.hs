module Arkham.SkillTest.Base where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Classes.Entity
import Arkham.Card
import Arkham.Card.Id
import Arkham.Id
import Arkham.Json
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token

data SkillTest = SkillTest
  { skillTestInvestigator :: InvestigatorId
  , skillTestSkillType :: SkillType
  , skillTestDifficulty :: Int
  , skillTestSetAsideTokens :: [Token]
  , skillTestRevealedTokens :: [Token] -- tokens may change from physical representation
  , skillTestResolvedTokens :: [Token]
  , skillTestValueModifier :: Int
  , skillTestResult :: SkillTestResult
  , skillTestCommittedCards :: HashMap CardId (InvestigatorId, Card)
  , skillTestSource :: Source
  , skillTestTarget :: Target
  , skillTestAction :: Maybe Action
  , skillTestSubscribers :: [Target]
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
  parseJSON = genericParseJSON $ aesonOptions $ Just "skillTest"

instance TargetEntity SkillTest where
  toTarget _ = SkillTestTarget
  isTarget _ SkillTestTarget = True
  isTarget _ _ = False

instance SourceEntity SkillTest where
  toSource SkillTest {..} =
    SkillTestSource
      skillTestInvestigator
      skillTestSkillType
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
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

