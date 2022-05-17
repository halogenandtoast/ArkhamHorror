module Arkham.SkillTest.Base where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Card
import Arkham.Card.Id
import Arkham.Id
import Arkham.Json
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import {-# SOURCE #-} Arkham.Target
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

instance ToJSON SkillTest where
  toJSON = genericToJSON $ aesonOptions $ Just "skillTest"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skillTest"

instance FromJSON SkillTest where
  parseJSON = genericParseJSON $ aesonOptions $ Just "skillTest"
