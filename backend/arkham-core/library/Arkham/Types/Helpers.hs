module Arkham.Types.Helpers where

import Arkham.Json
import ClassyPrelude hiding (unpack)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Random
import Data.Foldable (foldrM)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d) = f a b c d

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

concatMapM'
  :: (Monad m, MonoFoldable mono) => (Element mono -> m [b]) -> mono -> m [b]
concatMapM' f xs = concatMapM f (toList xs)

foldTokens :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldTokens s tokens f = foldrM (flip f) s tokens

count :: (a -> Bool) -> [a] -> Int
count = (length .) . filter

sample :: MonadRandom m => NonEmpty a -> m a
sample xs = do
  idx <- getRandomR (0, NE.length xs - 1)
  pure $ xs NE.!! idx

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

data With a b = With a b

instance (ToJSON a, ToJSON b) => ToJSON (a `With` b) where
  toJSON (a `With` b) = case (toJSON a, toJSON b) of
    (Object o, Object m) -> Object $ HashMap.union m o
    (a', b') -> metadataError a' b'
   where
    metadataError a' b' =
      error
        . unpack
        . toLazyText
        $ "With failed to serialize to object: "
        <> "\nattrs: "
        <> encodeToTextBuilder a'
        <> "\nmetadata: "
        <> encodeToTextBuilder b'

instance (FromJSON a, FromJSON b) => FromJSON (a `With` b) where
  parseJSON = withObject "With"
    $ \o -> With <$> parseJSON (Object o) <*> parseJSON (Object o)

instance (Show a, Show b) => Show (a `With` b) where
  show (With a b) = show a <> " WITH " <> show b

with :: a -> b -> a `With`  b
with a b = With a b
