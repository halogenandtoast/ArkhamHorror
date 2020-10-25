module Arkham.Types.TokenId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype TokenId = TokenId { unTokenId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
