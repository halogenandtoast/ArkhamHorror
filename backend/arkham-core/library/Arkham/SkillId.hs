module Arkham.SkillId where

import Arkham.Prelude

import Arkham.Card.Id

newtype SkillId = SkillId { unSkillId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype CommittedSkillId = CommittedSkillId { unCommitedSkillId :: SkillId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)
