module Arkham.Helpers.Message where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Draw.Types
import Arkham.Id
import Arkham.Message

drawCards
  :: (MonadRandom m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCards i source n = do
  drawing <- newCardDraw i source n
  pure $ DrawCards drawing

drawCardsAction
  :: (MonadRandom m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCardsAction i source n = do
  drawing <- newCardDraw i source n
  pure $ DrawCards $ asDrawAction drawing
