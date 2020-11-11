module Arkham.Types.EffectId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype EffectId = EffectId { unEffectId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
