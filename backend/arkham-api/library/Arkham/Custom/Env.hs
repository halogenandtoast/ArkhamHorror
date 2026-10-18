{- | The values a custom card's JSON is written against.

Steps and specs both refer to things by @$name@ and both have to compare and
decode what comes back, so the substitution lives here rather than in either.
-}
module Arkham.Custom.Env where

import Arkham.Prelude
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Char (isAlphaNum)
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
  -- An empty one is a field the editor left behind, not a label of no words:
  -- every caller here is a name or a prompt, and none of them means "blank".
  Just (String t) | not (null t) -> ownText env t
  _ -> fallback

{- | A card's own text, as the key the client looks it up by.

@$.again@ is "the text this card calls @again@". Written out it would be
@$custom.<the card's code>.again@, which is the same thing said in a way that
breaks when the card is renamed and that nobody would type. The card code is
what scopes it, so two sets may both have an @again@ and neither has to know.

Anything else is left exactly as written: a label with no @$@ is words, and a
@$@ key that names a scope of its own means that scope.
-}
ownText :: Env -> Text -> Text
ownText env t = inlineBindings env $ case T.stripPrefix "$." t of
  Nothing -> t
  Just rest -> case KeyMap.lookup "cardCode" env >>= parseMaybe parseJSON of
    Just (code :: Text) -> "$" <> customTextScope <> "." <> code <> "." <> rest
    Nothing -> t

{- | What a name in scope stands for, inside a prompt.

A label is not decoded, it is sent as it is written, so 'substitute' never
reaches the @$name@ in @count=i:$repeat@ -- it only replaces a string that is
nothing but a binding. This replaces them where they stand, which is what lets a
prompt say how many of something there are.

A name followed by a dot is left alone: @$label.addCurseTokens@ is a key the
client looks up, and its first word is not a binding however much it looks like
one. A name nothing bound is left alone too, for the same reason -- every
@$key@ in a label is exactly that shape.
-}
inlineBindings :: Env -> Text -> Text
inlineBindings env = go
 where
  go text =
    let (before, rest) = T.breakOn "$" text
     in case T.uncons rest of
          Nothing -> before
          Just (_, after) ->
            let (name, remainder) = T.span nameChar after
             in before <> stands name remainder <> go remainder
  nameChar c = isAlphaNum c || c == '_'
  stands name remainder
    | T.null name = "$"
    | Just ('.', _) <- T.uncons remainder = "$" <> name
    | otherwise = fromMaybe ("$" <> name) (KeyMap.lookup (Key.fromText name) env >>= scalar)
  -- A whole number reads as one; anything else a prompt cannot say, so the
  -- name stands rather than a figure nobody wrote appearing in its place.
  scalar v = case v of
    String text -> Just text
    Number _ -> tshow <$> (parseMaybe parseJSON v :: Maybe Int)
    _ -> Nothing

-- | Where a custom card's own text is merged into the client's messages.
customTextScope :: Text
customTextScope = "custom"
