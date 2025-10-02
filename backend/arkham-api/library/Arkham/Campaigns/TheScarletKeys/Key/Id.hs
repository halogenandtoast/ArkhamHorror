module Arkham.Campaigns.TheScarletKeys.Key.Id where

import Arkham.Prelude
import Arkham.Card.CardCode
import Arkham.Id

instance AsId KeyId where
  type IdOf KeyId = KeyId
  asId = id

newtype KeyId = KeyId { unKeyId :: CardCode }
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString, HasCardCode)
