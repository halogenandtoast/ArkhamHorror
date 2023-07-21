module Arkham.Helpers.Message.Discard where

import Arkham.Prelude

import Arkham.Card
import Arkham.Discard
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Source

discardFromHand
  :: Sourceable source
  => InvestigatorId
  -> source
  -> DiscardStrategy
  -> Int
  -> HandDiscard Message
discardFromHand iid (toSource -> source) strategy amount =
  HandDiscard
    { discardStrategy = strategy
    , discardFilter = AnyCard
    , discardSource = source
    , discardAmount = amount
    , discardThen = Nothing
    , discardTarget = Nothing
    , discardInvestigator = iid
    }

chooseAndDiscardCard
  :: Sourceable source => InvestigatorId -> source -> HandDiscard Message
chooseAndDiscardCard iid source = discardFromHand iid source DiscardChoose 1

discardCard
  :: (Sourceable source, IsCard card)
  => InvestigatorId
  -> source
  -> card
  -> HandDiscard Message
discardCard iid source (toCardId -> cardId) =
  (chooseAndDiscardCard iid source) {discardFilter = CardWithId cardId}

randomDiscard
  :: Sourceable source => InvestigatorId -> source -> HandDiscard Message
randomDiscard iid source = randomDiscardMatching iid source AnyCard

randomDiscardN
  :: Sourceable source => InvestigatorId -> source -> Int -> HandDiscard Message
randomDiscardN iid source n = (randomDiscard iid source) {discardAmount = n}

randomDiscardMatching
  :: Sourceable source
  => InvestigatorId
  -> source
  -> CardMatcher
  -> HandDiscard Message
randomDiscardMatching iid source matcher =
  (discardFromHand iid source DiscardRandom 1) {discardFilter = matcher}
