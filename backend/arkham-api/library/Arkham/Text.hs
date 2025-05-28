{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Text where

import Arkham.Card.CardCode
import Arkham.I18n
import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH

newtype Tooltip = Tooltip Text
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, Ord)

data FlavorTextModifier
  = BlueEntry
  | GreenEntry
  | RightAligned
  | PlainText
  | InvalidEntry
  | ValidEntry
  | ResolutionEntry
  | CheckpointEntry
  | InterludeEntry
  | NestedEntry
  deriving stock (Show, Eq, Ord, Data)

data ListItemEntry = ListItemEntry
  { entry :: FlavorTextEntry
  , nested :: [ListItemEntry]
  }
  deriving stock (Show, Eq, Ord, Data)

data FlavorTextEntry
  = BasicEntry {text :: Text}
  | I18nEntry {key :: Text, variables :: Map Text Value}
  | HeaderEntry {key :: Text}
  | ModifyEntry
      { modifiers :: [FlavorTextModifier]
      , entry :: FlavorTextEntry
      }
  | CompositeEntry
      { entries :: [FlavorTextEntry]
      }
  | ColumnEntry
      { entries :: [FlavorTextEntry]
      }
  | ListEntry {list :: [ListItemEntry]}
  | CardEntry {cardCode :: CardCode, imageModifiers :: [ImageModifier]}
  | EntrySplit
  deriving stock (Show, Eq, Ord, Data)

data ImageModifier = RemoveImage | SelectImage
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup FlavorTextEntry where
  CompositeEntry entries1 <> CompositeEntry entries2 = CompositeEntry (entries1 <> entries2)
  CompositeEntry entries1 <> entry2 = CompositeEntry (entries1 <> [entry2])
  entry1 <> CompositeEntry entries2 = CompositeEntry (entry1 : entries2)
  ListEntry entries1 <> ListEntry entries2 = ListEntry (entries1 <> entries2)
  entry1 <> entry2 = CompositeEntry [entry1, entry2]

data FlavorText = FlavorText
  { flavorTitle :: Maybe Text
  , flavorBody :: [FlavorTextEntry]
  }
  deriving stock (Show, Eq, Ord, Data)

mapFlavorText :: (FlavorTextEntry -> FlavorTextEntry) -> FlavorText -> FlavorText
mapFlavorText f (FlavorText title entries) =
  FlavorText title (map f entries)

addFlavorEntry :: FlavorText -> FlavorTextEntry -> FlavorText
addFlavorEntry (FlavorText title entries) entry' =
  FlavorText title (entries <> [entry'])

setFlavorTitle :: Text -> FlavorText -> FlavorText
setFlavorTitle title (FlavorText _ entries) = FlavorText (Just title) entries

instance Semigroup FlavorText where
  FlavorText mTitle1 body1 <> FlavorText mTitle2 body2 = FlavorText (mTitle1 <|> mTitle2) (body1 <> body2)

instance Monoid FlavorText where
  mempty = FlavorText Nothing []

i18n :: HasI18n => Text -> FlavorText
i18n = FlavorText Nothing . pure . i18nEntry

i18nEntry :: HasI18n => Scope -> FlavorTextEntry
i18nEntry t = I18nEntry (intercalate "." (?scope <> [t])) ?scopeVars

i18nWithTitle :: HasI18n => Text -> FlavorText
i18nWithTitle t = FlavorText (Just $ toI18n $ t <> ".title") [i18nEntry $ t <> ".body"]

toI18n :: HasI18n => Text -> Text
toI18n = ("$" <>) . ikey

toFlavor :: FlavorTextEntry -> FlavorText
toFlavor = FlavorText Nothing . pure

ft :: Text -> FlavorText
ft = FlavorText Nothing . pure . BasicEntry

mconcat
  [ deriveJSON defaultOptions ''FlavorTextModifier
  , deriveToJSON defaultOptions ''ImageModifier
  , [d|
      instance FromJSON ImageModifier where
        parseJSON (String s) = pure $ case s of
          "RemoveImage" -> RemoveImage
          "SelectImage" -> SelectImage
          _ -> error $ "Unknown image modifier: " <> show s
        parseJSON _ = pure RemoveImage
      |]
  , deriveJSON defaultOptions ''ListItemEntry
  , deriveToJSON defaultOptions ''FlavorTextEntry
  , [d|
      instance FromJSON FlavorTextEntry where
        parseJSON (String s) = pure $ BasicEntry s
        parseJSON o = $(mkParseJSON defaultOptions ''FlavorTextEntry) o
      |]
  , deriveJSON (aesonOptions $ Just "flavor") ''FlavorText
  ]
