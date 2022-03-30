module Arkham.Prelude
  ( module X
  , module Arkham.Prelude
  ) where

import ClassyPrelude as X hiding (on, (\\))

import Control.Lens as X
  ( Lens'
  , Traversal'
  , at
  , ix
  , lens
  , preview
  , to
  , traverseOf
  , traverseOf_
  , view
  , views
  , (%~)
  , (&)
  , (+~)
  , (-~)
  , (.~)
  , (<>~)
  , (?~)
  , (^.)
  , (^..)
  , (^?)
  )
import Control.Lens.TH as X
import Control.Monad.Extra as X
  (allM, andM, anyM, concatMapM, fromMaybeM, mapMaybeM, mconcatMapM)
import Control.Monad.Random as X (MonadRandom)
import Control.Monad.Random.Class as X (getRandom, getRandomR, getRandoms)
import Control.Monad.Random.Strict as X (Random)
import Data.Aeson as X
import Data.Aeson.Text
import Data.Char qualified as C
import Data.Coerce as X (coerce)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List as X (nub, (\\))
import Data.List qualified as L
import Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder
import Data.UUID as X (UUID)
import GHC.Stack as X
import Language.Haskell.TH hiding (location)
import Safe as X (fromJustNote)
import System.Random.Shuffle as X

suffixedNamer :: FieldNamer
suffixedNamer _ _ n = case dropWhile C.isLower (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedWithNamer :: String -> FieldNamer
suffixedWithNamer str _ _ n = case drop (length str) (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedWithFields :: String -> LensRules
suffixedWithFields suffix =
  defaultFieldRules & lensField .~ suffixedWithNamer suffix

suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer

guardM :: (Alternative m, Monad m) => m Bool -> m ()
guardM p = p >>= guard

mapSet :: (Hashable b, Eq b) => (a -> b) -> HashSet a -> HashSet b
mapSet = HashSet.map

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

mapFrom
  :: IsMap map => (MapValue map -> ContainerKey map) -> [MapValue map] -> map
mapFrom f = mapFromList . map (toFst f)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

traverseToSnd :: Functor m => (a -> m b) -> a -> m (a, b)
traverseToSnd f a = (a, ) <$> f a

maxes :: [(a, Int)] -> [a]
maxes ps = case sortedPairs of
  [] -> []
  ((_, c) : _) -> map fst $ takeWhile ((== c) . snd) sortedPairs
  where sortedPairs = sortOn (Down . snd) ps

mins :: [(a, Int)] -> [a]
mins ps = case sortedPairs of
  [] -> []
  ((_, c) : _) -> map fst $ takeWhile ((== c) . snd) sortedPairs
  where sortedPairs = sortOn snd ps

concatMapM'
  :: (Monad m, MonoFoldable mono) => (Element mono -> m [b]) -> mono -> m [b]
concatMapM' f xs = concatMapM f (toList xs)

count :: (a -> Bool) -> [a] -> Int
count = (length .) . filter

countM :: Monad m => (a -> m Bool) -> [a] -> m Int
countM = (fmap length .) . filterM

none :: MonoFoldable mono => (Element mono -> Bool) -> mono -> Bool
none = (not .) . any

noneM
  :: (Monad m, MonoFoldable mono) => (Element mono -> m Bool) -> mono -> m Bool
noneM f xs = not <$> anyM f (otoList xs)

notNull :: MonoFoldable mono => mono -> Bool
notNull = not . null

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

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d) = f a b c d

cycleN :: Int -> [a] -> [a]
cycleN n as = take (length as * n) $ L.cycle as

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst a = deleteFirstMatch (== a)

deleteFirstMatch :: (a -> Bool) -> [a] -> [a]
deleteFirstMatch _ [] = []
deleteFirstMatch f (a' : as) | f a' = as
deleteFirstMatch f (b' : as) = b' : deleteFirstMatch f as

data With a b = With a b

instance (Eq a, Eq b) => Eq (With a b) where
  With a1 b1 == With a2 b2 = a1 == a2 && b1 == b2

instance (ToJSON a, ToJSON b) => ToJSON (a `With` b) where
  toJSON (a `With` b) = case (toJSON a, toJSON b) of
    (Object o, Object m) -> Object $ KeyMap.union m o
    (a', b') -> metadataError a' b'
   where
    metadataError a' b' =
      error
        . TL.unpack
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
with = With

withBase :: a `With` b -> a
withBase (a `With` _) = a
