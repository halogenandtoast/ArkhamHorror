module Arkham.ActId where

import Arkham.Prelude

import Arkham.Card.CardCode

newtype ActStep = ActStep { unActStep :: Int }

newtype ActId = ActId { unActId :: CardCode }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)
