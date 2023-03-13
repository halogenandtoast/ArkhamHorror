module Arkham.Discard where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity.Source
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Target

data DiscardStrategy = DiscardChoose | DiscardRandom
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HandDiscard = HandDiscard
  { discardStrategy :: DiscardStrategy
  , discardInvestigator :: InvestigatorId
  , discardSource :: Source
  , discardTarget :: Maybe Target
  , discardFilter :: CardMatcher
  , discardAmount :: Int
  , discardThen :: Maybe Message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsMessage HandDiscard where
  toMessage = DiscardFromHand
  {-# INLINE toMessage #-}

discardFromHand
  :: Sourceable source
  => InvestigatorId
  -> source
  -> DiscardStrategy
  -> Int
  -> HandDiscard
discardFromHand iid (toSource -> source) strategy amount = HandDiscard
  { discardStrategy = strategy
  , discardFilter = AnyCard
  , discardSource = source
  , discardAmount = amount
  , discardThen = Nothing
  , discardTarget = Nothing
  , discardInvestigator = iid
  }

chooseAndDiscardCard
  :: Sourceable source => InvestigatorId -> source -> HandDiscard
chooseAndDiscardCard iid source = discardFromHand iid source DiscardChoose 1

discardCard
  :: (Sourceable source, IsCard card)
  => InvestigatorId
  -> source
  -> card
  -> HandDiscard
discardCard iid source (toCardId -> cardId) =
  (chooseAndDiscardCard iid source) { discardFilter = CardWithId cardId }

randomDiscard
  :: Sourceable source => InvestigatorId -> source -> HandDiscard
randomDiscard iid source = randomDiscardMatching iid source AnyCard

randomDiscardMatching
  :: Sourceable source
  => InvestigatorId
  -> source
  -> CardMatcher
  -> HandDiscard
randomDiscardMatching iid source matcher =
  (discardFromHand iid source DiscardRandom 1) { discardFilter = matcher }
