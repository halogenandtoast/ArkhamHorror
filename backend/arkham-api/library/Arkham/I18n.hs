{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Arkham.I18n where

import Arkham.Prelude hiding (intercalate)
import Data.Text (intercalate)

type Scope = Text
type HasI18n = (?scope :: [Scope])

withI18n :: (HasI18n => a) -> a
withI18n a = let ?scope = ([] :: [Scope]) in a

scope :: HasI18n => Scope -> (HasI18n => a) -> a
scope t a = let ?scope = ?scope <> [t] in a

ikey :: HasI18n => Scope -> Text
ikey t = intercalate "." $ ?scope <> [t]
