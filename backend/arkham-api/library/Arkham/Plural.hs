module Arkham.Plural (pluralize, pluralize_) where

import Arkham.Prelude hiding (toLower, toUpper)
import Data.Char (isUpper, toLower, toUpper)
import Data.Text qualified as T

pluralize :: Int -> Text -> Text
pluralize n noun = tshow n <> " " <> pluralize_ n noun

pluralize_ :: Int -> Text -> Text
pluralize_ n noun
  | n == 1 = noun
  | otherwise = pluralForm noun

pluralForm :: Text -> Text
pluralForm w =
  let lw = T.toLower w
      p = pluralLower lw
   in preserveCase w p

pluralLower :: Text -> Text
pluralLower w =
  case w of
    _
      | T.isSuffixOf "y" w
      , Just c <- charBeforeSuffix 1 w
      , isConsonant c ->
          T.dropEnd 1 w <> "ies"
      | endsWithAny ["s", "x", "z", "ch", "sh"] w -> w <> "es"
      | otherwise -> w <> "s"

endsWithAny :: [Text] -> Text -> Bool
endsWithAny ss t = any (`T.isSuffixOf` t) ss

charBeforeSuffix :: Int -> Text -> Maybe Char
charBeforeSuffix k t =
  let n = T.length t
   in if n > k then Just (T.index t (n - k - 1)) else Nothing

isConsonant :: Char -> Bool
isConsonant (toLower -> c) =
  c >= 'a' && c <= 'z' && c `notElem` ("aeiou" :: String)

preserveCase :: Text -> Text -> Text
preserveCase original lowerPlural =
  case T.uncons original of
    Nothing -> lowerPlural
    Just (c, _)
      | isUpper c -> titleWords lowerPlural
      | otherwise -> lowerPlural

titleWords :: Text -> Text
titleWords =
  T.unwords
    . map titleWord
    . T.words

titleWord :: Text -> Text
titleWord t =
  case T.uncons t of
    Nothing -> t
    Just (c, r) -> T.cons (toUpper c) r
