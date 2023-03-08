{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.SkillTest (
  module X,
  module Arkham.SkillTest,
) where

import Arkham.Prelude

import Arkham.SkillTestResult
import Arkham.SkillTest.Base as X
import Arkham.SkillTest.Type as X
import Arkham.Helpers.SkillTest as X
import Arkham.Card
import Arkham.Id
import Arkham.Target
import Arkham.Token

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

committedCardsL :: Lens' SkillTest (HashMap InvestigatorId [Card])
committedCardsL =
  lens skillTestCommittedCards $ \m x -> m {skillTestCommittedCards = x}

resultL :: Lens' SkillTest SkillTestResult
resultL = lens skillTestResult $ \m x -> m {skillTestResult = x}

typeL :: Lens' SkillTest SkillTestType
typeL = lens skillTestType $ \m x -> m {skillTestType = x}

baseValueL :: Lens' SkillTest SkillTestBaseValue
baseValueL = lens skillTestBaseValue $ \m x -> m {skillTestBaseValue = x}

valueModifierL :: Lens' SkillTest Int
valueModifierL =
  lens skillTestValueModifier $ \m x -> m {skillTestValueModifier = x}
