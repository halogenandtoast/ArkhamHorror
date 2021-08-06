module Arkham.Types.SkillId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype SkillId = SkillId { unSkillId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)

newtype CommittedSkillId = CommittedSkillId { unCommitedSkillId :: SkillId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)
