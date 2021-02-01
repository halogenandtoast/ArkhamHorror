module Arkham.Types.SkillId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype SkillId = SkillId { unSkillId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)
