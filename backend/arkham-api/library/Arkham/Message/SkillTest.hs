{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.SkillTest where

import Arkham.Ability.Types (AbilityRef)
import Arkham.Action (Action)
import Arkham.Card (Card)
import Arkham.Id
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Question (UI)
import {-# SOURCE #-} Arkham.SkillTest.Base
  ( SkillTest
  , SkillTestBaseValue
  , SkillTestDifficulty
  , SkillTestResultsData
  )
import Arkham.SkillTest.Option (SkillTestOption)
import Arkham.SkillTest.Type (SkillTestType)
import Arkham.SkillTestResult qualified as SkillTest
import Arkham.SkillType (SkillType)
import Arkham.Source (Source)
import Arkham.Target (Target)
import Data.Aeson.TH

newtype FromSkillType = FromSkillType SkillType
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype ToSkillType = ToSkillType SkillType
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- | Messages relating to skill-test setup, resolution, commit/uncommit, and
-- skill-test windows. Routed by 'SkillTestId' for the most part — i.e., the
-- in-progress test the message refers to.
data SkillTestMessage
  = AbilityIsSkillTest_ AbilityRef
  | CollectSkillTestOptions_
  | NextSkillTest_ SkillTestId
  | BeforeSkillTest_ SkillTestId
  | ChangeSkillTestType_ SkillTestType SkillTestBaseValue
  | IncreaseSkillTestDifficulty_ Int
  | ReplaceSkillTestSkill_ FromSkillType ToSkillType
  | RepeatSkillTest_ SkillTestId SkillTestId
  | SetSkillTestTarget_ Target
  | SetSkillTestResolveFailureInvestigator_ InvestigatorId
  | BeginSkillTestWithPreMessages_ Bool [Message] SkillTest
  | BeginSkillTestWithPreMessages'_ [Message] SkillTest
  | BeginSkillTestAfterFast_
  | CommitCard_ InvestigatorId Card
  | CommitToSkillTest_ SkillTestId (UI Message)
  | FailSkillTest_
  | FailedSkillTest_ InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | PassSkillTest_
  | PassSkillTestBy_ Int
  | PassedSkillTest_ InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | RerunSkillTest_
  | RevelationSkillTest_ SkillTestId InvestigatorId Source SkillType SkillTestDifficulty
  | RunSkillTest_ InvestigatorId
  | AfterThisTestResolves_ SkillTestId [Message]
  | StartSkillTest_ InvestigatorId
  | Successful_ (Action, Target) InvestigatorId Source Target Int
  | Failed_ (Action, Target) InvestigatorId Source Target Int
  | TriggerSkillTest_ InvestigatorId
  | SkillTestResultOption_ SkillTestOption
  | SkillTestResultOptions_ [SkillTestOption]
  | RecalculateSkillTestResults_
  | RecalculateSkillTestResultsCanChangeAutomatic_ Bool
  | SkillTestApplyResults_
  | SkillTestApplyResultsAfter_
  | SkillTestAsk_ Message
  | SkillTestCommitCard_ InvestigatorId Card
  | SkillTestEnds_ SkillTestId InvestigatorId Source
  | SkillTestEnded_ SkillTestId
  | AfterSkillTestEnds_ Source Target SkillTest.SkillTestResult
  | SkillTestResults_ SkillTestResultsData
  | SkillTestUncommitCard_ InvestigatorId Card
  | AfterSkillTestQuiet_ [Message]
  | AfterSkillTestOption_ InvestigatorId Text [Message]
  | EndSkillTestWindow_
  | ReturnSkillTestRevealedChaosTokens_
  | RevealSkillTestChaosTokens_ InvestigatorId
  | RevealSkillTestChaosTokensAgain_ InvestigatorId
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''SkillTestMessage)
