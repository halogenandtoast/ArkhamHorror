module Arkham.Types.ActId where

import Arkham.Types.Card.CardCode
import ClassyPrelude
import Data.Aeson

newtype ActStep = ActStep { unActStep :: Int }

newtype ActId = ActId CardCode
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype AdvanceableActId = AdvanceableActId { unAdvanceableActId :: ActId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

