module Arkham.Types.TreacheryId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype TreacheryId = TreacheryId { unTreacheryId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype StoryTreacheryId = StoryTreacheryId { unStoryTreacheryId :: TreacheryId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
