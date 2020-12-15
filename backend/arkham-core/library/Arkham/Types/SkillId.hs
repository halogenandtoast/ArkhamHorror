module Arkham.Types.SkillId where

import Arkham.Prelude

newtype SkillId = SkillId { unSkillId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
