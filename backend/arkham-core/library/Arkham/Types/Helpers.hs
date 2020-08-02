module Arkham.Types.Helpers where

import ClassyPrelude hiding (unpack)
import Arkham.Json
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder

without :: Int -> [a] -> [a]
without n as = [ a | (i, a) <- zip [0 ..] as, i /= n ]

data With a b = With a b

instance (ToJSON a, ToJSON b) => ToJSON (a `With` b) where
  toJSON (a `With` b) = case (toJSON a, toJSON b) of
    (Object o, Object m) -> Object $ HashMap.union m o
    (a, b) -> metadataError a b
   where
    metadataError a b =
      error
        . unpack
        . toLazyText
        $ "With failed to serialize to object: "
        <> "\nattrs: "
        <> encodeToTextBuilder a
        <> "\nmetadata: "
        <> encodeToTextBuilder b

instance (FromJSON a, FromJSON b) => FromJSON (a `With` b) where
  parseJSON = withObject "With" $ \o ->
    With <$> parseJSON (Object o) <*> parseJSON (Object o)

instance (Show a, Show b) => Show (a `With` b) where
  show (With a b) = show a <> " WITH " <> show b

with :: a -> b -> a `With`  b
with a b = With a b

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0 = Nothing
  | otherwise = go i xs
 where
  go :: Int -> [a] -> Maybe a
  go 0 (x : _) = Just x
  go j (_ : ys) = go (j - 1) ys
  go _ [] = Nothing
{-# INLINE (!!?) #-}

fromSet :: (Eq key) => HashSet key -> HashMap key value -> [value]
fromSet hset =
  HashMap.foldrWithKey (\k v vs -> if k `elem` hset then v : vs else vs) []

drawCard :: [a] -> (Maybe a, [a])
drawCard [] = (Nothing, [])
drawCard (x : xs) = (Just x, xs)

newtype Deck a = Deck { unDeck :: [a] }
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Show (Deck a) where
  show _ = "<Deck>"

newtype Bag a = Bag { unBag :: [a] }
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Show (Bag a) where
  show _ = "<Bag>"
