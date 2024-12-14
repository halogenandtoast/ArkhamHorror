{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Arkham.I18n where

import Arkham.Prelude hiding (intercalate)
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Text (intercalate)

type Scope = Text
type HasI18n = (?scope :: [Scope], ?scopeVars :: Map Text Value)

withI18n :: (HasI18n => a) -> a
withI18n a = let ?scope = ([] :: [Scope]); ?scopeVars = (mempty :: Map Text Value) in a

scope :: HasI18n => Scope -> (HasI18n => a) -> a
scope t a = let ?scope = ?scope <> [t] in a

unscoped :: HasI18n => (HasI18n => a) -> a
unscoped a = let ?scope = [] in a

ikey :: HasI18n => Scope -> Text
ikey t = intercalate "." (?scope <> [t]) <> varStr
 where
  varStr = case map toVarPair (mapToList ?scopeVars) of
    [] -> ""
    vs -> " " <> intercalate " " vs
  toVarPair (k, v) = k <> "=" <> toVar v
  toVar = \case
    Number n -> "i:" <> tshow n
    String n -> "s:" <> tshow n
    _ -> ""

withVar :: HasI18n => Text -> Value -> (HasI18n => a) -> a
withVar k v a = let ?scopeVars = ?scopeVars <> singletonMap k v in a

withVars :: HasI18n => [Pair] -> (HasI18n => a) -> a
withVars kv a = let ?scopeVars = ?scopeVars <> (Map.fromList $ map (bimap K.toText id) kv) in a
