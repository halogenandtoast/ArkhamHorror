{- | The values a custom card's JSON is written against.

Steps and specs both refer to things by @$name@ and both have to compare and
decode what comes back, so the substitution lives here rather than in either.
-}
module Arkham.Custom.Env where

import Arkham.Prelude
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Text qualified as T

type Env = KeyMap.KeyMap Value

-- * Substitution

{- | Replace every @"$name"@ string with the bound value, anywhere in the
structure. A whole string is replaced by a whole value, so a binding keeps its
type instead of being spliced into text.
-}
substitute :: Env -> Value -> Value
substitute env = go
 where
  go = \case
    String t | Just name <- T.stripPrefix "$" t -> fromMaybe (String t) (KeyMap.lookup (Key.fromText name) env)
    Object o -> Object (fmap go o)
    Array xs -> Array (fmap go xs)
    v -> v

{- | Equality for a @requires@ pair, with the two spellings of a nullary
constructor treated as one.

Aeson's tagged encoding leaves @contents@ off a constructor that has no fields,
so a placement comes back as @{"tag": "Limbo"}@, while the editor and anyone
writing the JSON by hand reach for @{"tag": "Limbo", "contents": []}@. Compared
raw, a requirement that looks exactly right never holds.
-}
sameValue :: Value -> Value -> Bool
sameValue a b = normalize a == normalize b
 where
  normalize = \case
    Object o
      | Just (Array xs) <- KeyMap.lookup "contents" o
      , null xs
      , KeyMap.member "tag" o ->
          Object (fmap normalize (KeyMap.delete "contents" o))
    Object o -> Object (fmap normalize o)
    Array xs -> Array (fmap normalize xs)
    v -> v

decodeWith :: FromJSON a => Env -> Value -> Maybe a
decodeWith env = parseMaybe parseJSON . substitute env

specObject :: Value -> KeyMap.KeyMap Value
specObject = \case
  Object o -> o
  _ -> mempty

-- | Does this value appear anywhere inside that one?
isSubValue :: Value -> Value -> Bool
isSubValue needle haystack = go haystack
 where
  go v
    | v == needle = True
    | otherwise = case v of
        Object o -> any go (KeyMap.elems o)
        Array xs -> any go xs
        _ -> False

-- * Steps

subSteps :: Value -> [Value]
subSteps v = fromMaybe [] (parseMaybe parseJSON v)

textField :: Env -> KeyMap.KeyMap Value -> Key.Key -> Text -> Text
textField env o key fallback = case substitute env <$> KeyMap.lookup key o of
  Just (String t) -> t
  _ -> fallback
