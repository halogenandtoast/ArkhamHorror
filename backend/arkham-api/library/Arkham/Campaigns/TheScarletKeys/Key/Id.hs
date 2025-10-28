module Arkham.Campaigns.TheScarletKeys.Key.Id where

import Arkham.Prelude
import Arkham.Card.CardCode
import Arkham.Id

instance AsId ScarletKeyId where
  type IdOf ScarletKeyId = ScarletKeyId
  asId = id

newtype ScarletKeyId = ScarletKeyId { unScarletKeyId :: CardCode }
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord, IsString, HasCardCode)
