module Arkham.Types.TreacheryId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype TreacheryId = TreacheryId { unTreacheryId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)

newtype StoryTreacheryId = StoryTreacheryId { unStoryTreacheryId :: TreacheryId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
