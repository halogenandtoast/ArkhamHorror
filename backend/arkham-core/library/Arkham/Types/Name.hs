module Arkham.Types.Name
  ( module Arkham.Types.Name
  )
where

import Arkham.Prelude
import Arkham.Types.Helpers

data Name = Name
  { nameTitle :: Text
  , nameSubtitle :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype LocationName = LocationName { unLocationName :: Name }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

nameToLabel :: Name -> Text
nameToLabel = pack . toLabel . replaceNonLetters . unpack . nameTitle
