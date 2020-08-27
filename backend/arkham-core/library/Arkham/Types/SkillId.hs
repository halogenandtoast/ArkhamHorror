module Arkham.Types.SkillId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype SkillId = SkillId { unSkillId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
