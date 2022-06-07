module Arkham.Name
  ( module Arkham.Name
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Helpers

data Name = Name
  { nameTitle :: Text
  , nameSubtitle :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSONKey, FromJSONKey, Hashable)

class Named a where
  toName :: a -> Name

instance Named a => Named (a `With` b) where
  toName (a `With` _) = toName a

instance IsString Name where
  fromString = mkName . fromString

display :: Name -> Text
display (Name title (Just subtitle)) = title <> ": " <> subtitle
display (Name title Nothing) = title

mkName :: Text -> Name
mkName = flip Name Nothing

mkFullName :: Text -> Text -> Name
mkFullName = (. Just) . Name

subtitled :: Text -> Text -> Name
subtitled = mkFullName

(<:>) :: Text -> Text -> Name
(<:>) = subtitled

instance ToJSON Name where
  toJSON = genericToJSON $ aesonOptions $ Just "name"
  toEncoding = genericToEncoding $ aesonOptions $ Just "name"

instance FromJSON Name where
  parseJSON = genericParseJSON $ aesonOptions $ Just "name"

nameToLabel :: Name -> Text
nameToLabel = pack . toLabel . replaceNonLetters . unpack . nameTitle
