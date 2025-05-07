{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Arkham.I18n where

import Arkham.Prelude hiding (intercalate)
import Arkham.SkillType
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

popScope :: HasI18n => (HasI18n => a) -> a
popScope a = let ?scope = reverse (drop 1 $ reverse ?scope) in a

unscoped :: HasI18n => (HasI18n => a) -> a
unscoped a = let ?scope = [] in a

ikey :: HasI18n => Scope -> Text
ikey t = intercalate "." (?scope <> [t]) <> varStr
 where
  varStr = case map toVarPair (mapToList ?scopeVars) of
    [] -> ""
    vs -> " " <> unwords vs
  toVarPair (k, v) = k <> "=" <> toVar v
  toVar = \case
    Number n -> "i:" <> tshow n
    String n -> "s:" <> tshow n
    _ -> ""

countVar :: HasI18n => Int -> (HasI18n => a) -> a
countVar = numberVar "count"

numberVar :: HasI18n => Text -> Int -> (HasI18n => a) -> a
numberVar var val a = withVar var (Number $ fromIntegral val) a

skillVar :: HasI18n => SkillType -> (HasI18n => a) -> a
skillVar v a = case v of
  SkillWillpower -> withVar "skill" (String "agility") a
  SkillIntellect -> withVar "skill" (String "intellect") a
  SkillCombat -> withVar "skill" (String "combat") a
  SkillAgility -> withVar "skill" (String "agility") a

skillIconVar :: HasI18n => SkillIcon -> (HasI18n => a) -> a
skillIconVar v a = case v of
  SkillIcon kind -> case kind of
    SkillWillpower -> withVar "skillIcon" (String "agility") a
    SkillIntellect -> withVar "skillIcon" (String "intellect") a
    SkillCombat -> withVar "skillIcon" (String "combat") a
    SkillAgility -> withVar "skillIcon" (String "agility") a
  WildIcon -> withVar "skillIcon" (String "wild") a
  WildMinusIcon -> withVar "skillIcon" (String "wildMinus") a

withVar :: HasI18n => Text -> Value -> (HasI18n => a) -> a
withVar k v a = let ?scopeVars = ?scopeVars <> singletonMap k v in a

withVars :: HasI18n => [Pair] -> (HasI18n => a) -> a
withVars kv a = let ?scopeVars = ?scopeVars <> Map.fromList (map (first K.toText) kv) in a
