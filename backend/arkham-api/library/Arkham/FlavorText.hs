{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.FlavorText (
  module X,
  flavorText,
  ul,
  li,
  compose,
  hr,
)
where

import Arkham.I18n as X
import Arkham.Text as X

import Arkham.Prelude
import Control.Monad.Writer
import Data.Text qualified as T
import GHC.Records

flavorText :: FlavorTextEntry -> FlavorText
flavorText = FlavorText Nothing . pure

type UlItems = Writer [ListItemEntry] ()

ul :: UlItems -> FlavorTextEntry
ul = ListEntry . execWriter

compose :: [FlavorTextEntry] -> FlavorTextEntry
compose = CompositeEntry

hr :: FlavorTextEntry
hr = EntrySplit

li :: HasI18n => Text -> UlItems
li t = tell [ListItemEntry (i18nEntry t) []]

specialize :: (Text -> UlItems) -> Text -> (ListItemEntry -> ListItemEntry) -> UlItems
specialize f t convert = tell $ map convert $ execWriter $ f t

instance HasField "nested" (Text -> UlItems) (String -> UlItems -> UlItems) where
  getField f = \t items -> specialize f (T.pack t) \case
    ListItemEntry entry nested -> ListItemEntry entry (nested <> execWriter items)

instance HasField "valid" (Text -> UlItems) (String -> UlItems) where
  getField f = \t -> specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (ValidEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [ValidEntry] entry) nested

instance HasField "invalid" (Text -> UlItems) (String -> UlItems) where
  getField f = \t -> specialize f (T.pack t) \case
    ListItemEntry entry nested -> case entry of
      ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (InvalidEntry : modifiers) inner) nested
      _ -> ListItemEntry (ModifyEntry [ValidEntry] entry) nested

instance HasField "validate" (Text -> UlItems) (Bool -> String -> UlItems) where
  getField f = \cond t ->
    let modifier = if cond then ValidEntry else InvalidEntry
     in specialize f (T.pack t) \case
          ListItemEntry entry nested -> case entry of
            ModifyEntry modifiers inner -> ListItemEntry (ModifyEntry (modifier : modifiers) inner) nested
            _ -> ListItemEntry (ModifyEntry [modifier] entry) nested
