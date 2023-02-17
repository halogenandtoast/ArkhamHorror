module Arkham.Prelude
  ( module X
  , module Arkham.Prelude
  ) where

import ClassyPrelude as X hiding ( foldlM, on, (\\) )
import Data.Type.Equality as X (type (~))

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
  ( allM, andM, anyM, concatMapM, fromMaybeM, mapMaybeM, mconcatMapM )
import Control.Monad.Random as X ( MonadRandom, uniform )
import Control.Monad.Random.Class as X ( getRandom, getRandomR, getRandoms )
import Control.Monad.Random.Strict as X ( Random )
import Data.Aeson as X hiding ( Result (..) )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text
import Data.Char qualified as C
import Data.Coerce as X ( coerce )
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Kind as X ( Type )
import Data.List as X ( nub, (\\) )
import Data.List qualified as L
import Data.List.NonEmpty as X ( NonEmpty (..), nonEmpty )
import Data.Semigroup as X ( Max (..), Min (..), Sum (..) )
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder
import Data.UUID as X ( UUID )
import GHC.Stack as X
import Language.Haskell.TH hiding ( location )
import Safe as X ( fromJustNote )
import System.Random.Shuffle as X

import Data.Foldable ( foldlM )

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

mapSet :: Hashable b => (a -> b) -> HashSet a -> HashSet b
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

traverseToSndM :: (Functor f, Functor m) => (a -> m (f b)) -> a -> m (f (a, b))
traverseToSndM f a = (a, ) <$$> f a

forToSnd :: (Traversable t, Applicative m) => t a -> (a -> m b) -> m (t (a, b))
forToSnd xs f = traverse (traverseToSnd f) xs

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
sample = uniform

sampleWithRest :: (Eq a, MonadRandom m) => NonEmpty a -> m (a, [a])
sampleWithRest xs = do
  x <- sample xs
  pure (x, deleteFirst x $ toList xs)

sampleN :: (Eq a, MonadRandom m) => Int -> NonEmpty a -> m [a]
sampleN 0 _ = pure []
sampleN _ (x :| []) = pure [x]
sampleN n xs = do
  (x, rest) <- sampleWithRest xs
  case nonEmpty rest of
    Nothing -> pure [x]
    Just xs' -> (x :) <$> sampleN (n - 1) xs'

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

findKey :: Hashable k => (v -> Bool) -> HashMap k v -> Maybe k
findKey p = fmap fst . find (p . snd) . mapToList

-- getMax will return a very low number
-- minBound :: Max Int
-- Max {getMax = -9223372036854775808}
-- so we clamp it to 0
getMax0 :: (Ord a, Num a) => Max a -> a
getMax0 current = getMax $ Max 0 <> current

foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w
foldMapM f = foldlM
  (\acc a -> do
    w <- f a
    return $! mappend acc w
  )
  mempty

frequencies :: Hashable a => [a] -> HashMap a Int
frequencies as = HashMap.map getSum $ foldr (unionWith (<>)) mempty $ map
  (`HashMap.singleton` (Sum 1))
  as

breakM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
breakM _ xs@[] = pure (xs, xs)
breakM p xs@(x : xs') = do
  b <- p x
  if b
    then pure ([], xs)
    else do
      (ys, zs) <- breakM p xs'
      pure (x : ys, zs)

(<$$>) :: (Functor f, Functor m) => (a -> b) -> m (f a) -> m (f b)
(<$$>) = fmap . fmap
infixl 4 <$$>

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

withIndex1 :: [a] -> [(Int, a)]
withIndex1 = zip [1..]
