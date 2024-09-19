{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Arkham.I18n where

import Arkham.Prelude hiding (intercalate)
import Data.Text (intercalate)

type Scope = Text
type HasI18n = (?scope :: [Scope], ?scopeVars :: Map Text Value)

withI18n :: (HasI18n => a) -> a
withI18n a = let ?scope = ([] :: [Scope]); ?scopeVars = (mempty :: Map Text Value) in a

scope :: HasI18n => Scope -> (HasI18n => a) -> a
scope t a = let ?scope = ?scope <> [t] in a

ikey :: HasI18n => Scope -> Text
ikey t = intercalate "." (?scope <> [t]) <> varStr
 where
  varStr = case map toVarPair (mapToList $ traceShowId ?scopeVars) of
    [] -> ""
    vs -> " " <> intercalate " " vs
  toVarPair (k, v) = k <> "=" <> toVar v
  toVar = \case
    Number n -> "i:" <> tshow n
    String n -> "s:" <> tshow n
    _ -> ""

withVar :: HasI18n => Text -> Value -> (HasI18n => a) -> a
withVar k v a = let ?scopeVars = ?scopeVars <> singletonMap k v in a
