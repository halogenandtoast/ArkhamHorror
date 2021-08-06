module Arkham.Types.TokenId where

import Arkham.Prelude

newtype TokenId = TokenId { unTokenId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
