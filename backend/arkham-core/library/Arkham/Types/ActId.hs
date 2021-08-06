module Arkham.Types.ActId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype ActStep = ActStep { unActStep :: Int }

newtype ActId = ActId { unActId :: CardCode }
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)
