module Arkham.Helpers.Message where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Id
import Arkham.Message

drawCards
  :: (Monad m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCards i source n = pure $ DrawCards i (toSource source) n False

drawCardsAction
  :: (Monad m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCardsAction i source n = pure $ DrawCards i (toSource source) n True
