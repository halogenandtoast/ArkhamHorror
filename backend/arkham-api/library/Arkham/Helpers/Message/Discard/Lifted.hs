module Arkham.Helpers.Message.Discard.Lifted (module Arkham.Discard, module Arkham.Helpers.Message.Discard.Lifted) where

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Discard
import Arkham.Helpers.Message.Discard qualified as Msg
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Source

discardFromHand
  :: (Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> DiscardStrategy
  -> Int
  -> m ()
discardFromHand iid source strategy amount = push . toMessage $ Msg.discardFromHand iid source strategy amount

chooseAndDiscardCard :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseAndDiscardCard iid source = push . toMessage $ Msg.chooseAndDiscardCard iid source

discardCard
  :: (Sourceable source, IsCard card, ReverseQueue m)
  => InvestigatorId
  -> source
  -> card
  -> m ()
discardCard iid source card = push . toMessage $ Msg.discardCard iid source card

randomDiscard
  :: (Sourceable source, ReverseQueue m) => InvestigatorId -> source -> m ()
randomDiscard iid source = push . toMessage $ Msg.randomDiscard iid source

randomDiscardN
  :: (Sourceable source, ReverseQueue m) => InvestigatorId -> source -> Int -> m ()
randomDiscardN iid source n = push . toMessage $ Msg.randomDiscardN iid source n

randomDiscardMatching
  :: (Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> CardMatcher
  -> m ()
randomDiscardMatching iid source matcher = push . toMessage $ Msg.randomDiscardMatching iid source matcher

discardAll
  :: (Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> CardMatcher
  -> m ()
discardAll iid source matcher = push . toMessage $ Msg.discardAll iid source matcher
