module Arkham.Helpers.Message where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Id
import Arkham.Message

drawCards
  :: SourceEntity source
  => InvestigatorId
  -> source
  -> Int
  -> Message
drawCards i source n = DrawCards i (toSource source) n False

drawCardsAction
  :: SourceEntity source
  => InvestigatorId
  -> source
  -> Int
  -> Message
drawCardsAction i source n = DrawCards i (toSource source) n True
