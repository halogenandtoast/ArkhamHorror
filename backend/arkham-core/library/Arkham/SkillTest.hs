{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.SkillTest (
  module X,
  module Arkham.SkillTest,
) where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.SkillTestResult
import Arkham.SkillTest.Base as X
import Arkham.SkillType
import Arkham.Classes.Entity
import Arkham.Card
import Arkham.Card.Id
import Arkham.Id
import {-# SOURCE #-} Arkham.Game
import Arkham.Source
import Arkham.Target
import Arkham.Token

class HasSkillTest m where
  getSkillTest :: m (Maybe SkillTest)

instance HasGame m => HasSkillTest m where
  getSkillTest = getGameSkillTest

getSkillTestTarget :: (Functor m, HasSkillTest m) => m (Maybe Target)
getSkillTestTarget = fmap skillTestTarget <$> getSkillTest

getSkillTestSource :: (Functor m, HasSkillTest m) => m (Maybe Source)
getSkillTestSource = fmap toSource <$> getSkillTest

data SkillTestResultsData = SkillTestResultsData
  { skillTestResultsSkillValue :: Int
  , skillTestResultsIconValue :: Int
  , skillTestResultsTokensValue :: Int
  , skillTestResultsDifficulty :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

subscribersL :: Lens' SkillTest [Target]
subscribersL =
  lens skillTestSubscribers $ \m x -> m {skillTestSubscribers = x}

setAsideTokensL :: Lens' SkillTest [Token]
setAsideTokensL =
  lens skillTestSetAsideTokens $ \m x -> m {skillTestSetAsideTokens = x}

resolvedTokensL :: Lens' SkillTest [Token]
resolvedTokensL =
  lens skillTestResolvedTokens $ \m x -> m {skillTestResolvedTokens = x}

revealedTokensL :: Lens' SkillTest [Token]
revealedTokensL =
  lens skillTestRevealedTokens $ \m x -> m {skillTestRevealedTokens = x}

committedCardsL :: Lens' SkillTest (HashMap CardId (InvestigatorId, Card))
committedCardsL =
  lens skillTestCommittedCards $ \m x -> m {skillTestCommittedCards = x}

resultL :: Lens' SkillTest SkillTestResult
resultL = lens skillTestResult $ \m x -> m {skillTestResult = x}

valueModifierL :: Lens' SkillTest Int
valueModifierL =
  lens skillTestValueModifier $ \m x -> m {skillTestValueModifier = x}

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

initSkillTest ::
  InvestigatorId ->
  Source ->
  Target ->
  Maybe Action ->
  SkillType ->
  Int ->
  Int ->
  SkillTest
initSkillTest iid source target maction skillType' _skillValue' difficulty' =
  SkillTest
    { skillTestInvestigator = iid
    , skillTestSkillType = skillType'
    , skillTestDifficulty = difficulty'
    , skillTestSetAsideTokens = mempty
    , skillTestRevealedTokens = mempty
    , skillTestResolvedTokens = mempty
    , skillTestValueModifier = 0
    , skillTestResult = Unrun
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestTarget = target
    , skillTestAction = maction
    , skillTestSubscribers = [InvestigatorTarget iid]
    }
