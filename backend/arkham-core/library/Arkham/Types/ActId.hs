module Arkham.Types.ActId where

import Arkham.Types.Card
import ClassyPrelude
import Data.Aeson

newtype ActId = ActId CardCode
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype AdvanceableActId = AdvanceableActId { unAdvanceableActId :: ActId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

