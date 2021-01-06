module Arkham.Types.Location.Name
  ( module Arkham.Types.Location.Name
  )
where

import Arkham.Prelude
import Arkham.Types.Helpers

data LocationName = LocationName
  { locationNameTitle :: Text
  , locationNameSubtitle :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

locationNameToLabel :: LocationName -> Text
locationNameToLabel =
  pack . toLabel . replaceNonLetters . unpack . locationNameTitle

