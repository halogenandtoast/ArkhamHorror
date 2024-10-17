module Arkham.SortedPair (SortedPair, sortedPair, unSortedPair, inSortedPair) where

import Arkham.Prelude
import Control.Monad.Fail
import Data.Aeson.Encoding (text)
import Data.Aeson.Encoding.Internal (econcat)
import Data.Aeson.Key qualified as K
import Data.Text qualified as T

newtype SortedPair a = SortedPair (a, a)
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

unSortedPair :: SortedPair a -> (a, a)
unSortedPair (SortedPair pair) = pair

inSortedPair :: Eq a => a -> SortedPair a -> Bool
inSortedPair a (SortedPair (b, c)) = a == b || a == c

instance (ToJSON a, ToJSONKey a) => ToJSONKey (SortedPair a) where
  toJSONKey = ToJSONKeyText toKey toKeyEncoding
   where
    toKey (SortedPair (a1, a2)) =
      mconcat [toKeyPart a1, K.fromString "--", toKeyPart a2]
    toKeyEncoding (SortedPair (a1, a2)) =
      econcat [encodePart a1, text "--", encodePart a2]
    toKeyPart = case toJSONKey of
      ToJSONKeyText f _ -> f
      _ -> error "ToJSONKey a must be ToJSONKeyText"
    encodePart = case toJSONKey of
      ToJSONKeyText _ enc -> enc
      _ -> error "ToJSONKey a must be ToJSONKeyText"

instance (FromJSON a, FromJSONKey a, Ord a) => FromJSONKey (SortedPair a) where
  fromJSONKey = FromJSONKeyTextParser parseKey
   where
    fromKeyPart = case fromJSONKey of
      FromJSONKeyText f -> pure . f
      FromJSONKeyTextParser f -> f
      _ -> \_ -> fail "FromJSONKey a must be ToJSONKeyText"
    parseKey key = case T.splitOn "--" key of
      [aTxt, bTxt] -> sortedPair <$> fromKeyPart aTxt <*> fromKeyPart bTxt
      _ -> fail "Invalid key format"

sortedPair :: Ord a => a -> a -> SortedPair a
sortedPair a b = SortedPair $ if a <= b then (a, b) else (b, a)
