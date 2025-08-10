{-# LANGUAGE TemplateHaskell #-}

module Arkham.Name (
  module Arkham.Name,
) where

import Arkham.Prelude

import Arkham.Classes.GameLogger
import Arkham.Helpers
import Arkham.Id
import Arkham.Json
import Data.Aeson.TH

data Name = Name
  { nameTitle :: Text
  , nameSubtitle :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Data)

class Named a where
  toName :: a -> Name

toTitle :: Named a => a -> Text
toTitle = nameTitle . toName
{-# INLINE toTitle #-}

hasTitle :: Named a => a -> Text -> Bool
hasTitle a title = toTitle a == title
{-# INLINE hasTitle #-}

instance Named Name where
  toName = id

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

nameToLabel :: Named a => a -> Text
nameToLabel = pack . toLabel . replaceNonLetters . unpack . toTitle

instance ToGameLoggerFormat Name where
  format = display

$(deriveJSON (aesonOptions $ Just "name") ''Name)

data Labeled a = Labeled
  { getLabel :: Name
  , unLabel :: a
  }
  deriving stock (Show, Eq, Ord, Data)

instance ToJSON a => ToJSON (Labeled a) where
  toJSON l = object ["getLabel" .= getLabel l, "unLabel" .= unLabel l]

instance FromJSON a => FromJSON (Labeled a) where
  parseJSON = withObject "Labeled" $ \o ->
    Labeled
      <$> o
      .: "getLabel"
      <*> o
      .: "unLabel"

labeled :: Named name => name -> a -> Labeled a
labeled (toName -> name) = Labeled name

instance ToGameLoggerFormat (With EnemyId Name) where
  format (With eid name) = "{enemy:\"" <> toTitle name <> "\":\"" <> tshow eid <> "\"}"

