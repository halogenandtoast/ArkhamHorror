
module Arkham.Types.Card.CardCode where

import ClassyPrelude
import Data.Aeson

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
