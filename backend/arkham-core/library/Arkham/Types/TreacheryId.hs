module Arkham.Types.TreacheryId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype TreacheryId = TreacheryId { unTreacheryId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
