module Arkham.Types.TreacheryId where

import Arkham.Prelude

newtype TreacheryId = TreacheryId { unTreacheryId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype StoryTreacheryId = StoryTreacheryId { unStoryTreacheryId :: TreacheryId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
