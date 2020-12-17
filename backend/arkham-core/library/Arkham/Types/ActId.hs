module Arkham.Types.ActId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype ActStep = ActStep { unActStep :: Int }

newtype ActId = ActId CardCode
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)
