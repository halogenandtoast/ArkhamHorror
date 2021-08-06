module Arkham.Types.Name
  ( module Arkham.Types.Name
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Helpers

data Name = Name
  { nameTitle :: Text
  , nameSubtitle :: Maybe Text
  }
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSONKey, FromJSONKey)

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

newtype LocationName = LocationName { unLocationName :: Name }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype EnemyName = EnemyName { unEnemyName :: Name }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype TreacheryName = TreacheryName { unTreacheryName :: Name }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

nameToLabel :: Name -> Text
nameToLabel = pack . toLabel . replaceNonLetters . unpack . nameTitle
