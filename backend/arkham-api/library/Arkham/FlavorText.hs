{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.FlavorText (
  module X,
  flavorText,
  ul,
  li,
  compose,
  h,
  hr,
  p,
  cols,
  img,
  UlItems,
)
where

import Arkham.I18n as X
import Arkham.Text as X

import Arkham.Card.CardCode
import Arkham.Prelude
import Control.Monad.Writer.Strict
import Data.Text qualified as T
import GHC.Records

flavorText :: FlavorTextEntry -> FlavorText
flavorText = FlavorText Nothing . pure

type UlItems = Writer [ListItemEntry] ()

ul :: UlItems -> FlavorTextEntry
ul = ListEntry . execWriter

h :: HasI18n => Scope -> FlavorTextEntry
h t = HeaderEntry (intercalate "." (?scope <> [t]))

p :: HasI18n => Scope -> FlavorTextEntry
p = i18nEntry

compose :: [FlavorTextEntry] -> FlavorTextEntry
compose = CompositeEntry

cols :: [FlavorTextEntry] -> FlavorTextEntry
cols = ColumnEntry

img :: CardCode -> FlavorTextEntry
img = (`CardEntry` [])

hr :: FlavorTextEntry
hr = EntrySplit

li :: HasI18n => Text -> UlItems
li t = tell [ListItemEntry (i18nEntry t) []]

specialize :: (Text -> UlItems) -> Text -> (ListItemEntry -> ListItemEntry) -> UlItems
specialize f t convert = tell $ map convert $ execWriter $ f t

instance HasField "nested" (Text -> UlItems) (String -> UlItems -> UlItems) where
  getField f t items = specialize f (T.pack t) \case
    ListItemEntry entry nested -> ListItemEntry entry (nested <> execWriter items)

instance HasField "valid" (Text -> UlItems) (String -> UlItems) where
  getField f t = specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (ValidEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [ValidEntry] entry) nested

instance HasField "invalid" (Text -> UlItems) (String -> UlItems) where
  getField f t = specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (InvalidEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [ValidEntry] entry) nested

instance HasField "validate" (Text -> UlItems) (Bool -> String -> UlItems) where
  getField f cond t =
    let modifier = if cond then ValidEntry else InvalidEntry
     in specialize f (T.pack t) \case
          ListItemEntry entry nested -> case entry of
            ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (modifier : modifiers) inner) nested
            _ -> ListItemEntry (ModifyEntry [modifier] entry) nested

instance HasField "remove" (CardCode -> FlavorTextEntry) (CardCode -> FlavorTextEntry) where
  getField f cardCode = case f cardCode of
    CardEntry _ modifiers -> CardEntry cardCode (RemoveImage : modifiers)
    other -> other

extendModifiers :: FlavorTextModifier -> FlavorTextEntry -> FlavorTextEntry
extendModifiers modifier = \case
  ModifyEntry modifiers inner -> ModifyEntry (modifier : modifiers) inner
  other -> ModifyEntry [modifier] other

instance HasField "blue" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers BlueEntry . f

instance HasField "right" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers RightAligned . f

instance HasField "nested" (Scope -> FlavorTextEntry) (Scope -> FlavorTextEntry) where
  getField f = extendModifiers NestedEntry . f

instance HasField "validate" (Scope -> FlavorTextEntry) (Bool -> Scope -> FlavorTextEntry) where
  getField f cond = extendModifiers (if cond then ValidEntry else InvalidEntry) . f
