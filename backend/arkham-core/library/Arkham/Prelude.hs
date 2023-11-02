module Arkham.Prelude (
  module X,
  module Arkham.Prelude,
) where

import ClassyPrelude as X hiding (foldlM, on, (\\))
import Data.Type.Equality as X (type (~))

import Control.Lens as X (
  Lens',
  Traversal',
  at,
  ix,
  lens,
  preview,
  to,
  traverseOf,
  traverseOf_,
  view,
  views,
  (%~),
  (&),
  (+~),
  (-~),
  (.~),
  (<>~),
  (?~),
  (^.),
  (^..),
  (^?),
 )
import Control.Lens.TH as X
import Control.Monad.Extra as X (
  allM,
  andM,
  anyM,
  concatForM,
  concatMapM,
  fromMaybeM,
  mapMaybeM,
  mconcatMapM,
  orM,
  partitionM,
 )
import Control.Monad.Random as X (MonadRandom, uniform)
import Control.Monad.Random.Class as X (getRandom, getRandomR, getRandoms)
import Control.Monad.Random.Strict as X (Random)
import Control.Monad.Trans.Maybe as X (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Aeson as X hiding (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text
import Data.Char qualified as C
import Data.Coerce as X (Coercible, coerce)
import Data.Data as X (Data)
import Data.Kind as X (Type)
import Data.List as X (nub, (\\))
import Data.List qualified as L
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Monoid.Extra as X (mwhen)
import Data.Semigroup as X (Max (..), Min (..), Sum (..))
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder
import Data.UUID as X (UUID)
import GHC.Stack as X
import Language.Haskell.TH hiding (location)
import Safe as X (fromJustNote)
import System.Random.Shuffle as X

import Data.Foldable (Foldable (foldMap), foldlM)
import Data.List.NonEmpty qualified as NE

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

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet = Set.map

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

mapFrom
  :: IsMap map => (MapValue map -> ContainerKey map) -> [MapValue map] -> map
mapFrom f = mapFromList . map (toFst f)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

traverseToSnd :: Functor m => (a -> m b) -> a -> m (a, b)
traverseToSnd f a = (a,) <$> f a

traverseToSndM :: (Functor f, Functor m) => (a -> m (f b)) -> a -> m (f (a, b))
traverseToSndM f a = (a,) <$$> f a

forToSnd :: (Traversable t, Applicative m) => t a -> (a -> m b) -> m (t (a, b))
forToSnd xs f = traverse (traverseToSnd f) xs

maxes :: [(a, Int)] -> [a]
maxes ps = case sortedPairs of
  [] -> []
  ((_, c) : _) -> map fst $ takeWhile ((== c) . snd) sortedPairs
 where
  sortedPairs = sortOn (Down . snd) ps

mins :: [(a, Int)] -> [a]
mins ps = case sortedPairs of
  [] -> []
  ((_, c) : _) -> map fst $ takeWhile ((== c) . snd) sortedPairs
 where
  sortedPairs = sortOn snd ps

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

unlessNull :: (Monad m, MonoFoldable mono) => mono -> m () -> m ()
unlessNull xs = unless (null xs)

sample :: MonadRandom m => NonEmpty a -> m a
sample xs = do
  idx <- getRandomR (0, NE.length xs - 1)
  pure $ xs NE.!! idx

sample2 :: MonadRandom m => a -> a -> m a
sample2 x y = sample (x :| [y])

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

eachWithRest :: Eq a => [a] -> [(a, [a])]
eachWithRest xs = [(x, deleteFirst x xs) | x <- xs]

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
  parseJSON = withObject "With" $
    \o -> With <$> parseJSON (Object o) <*> parseJSON (Object o)

instance (Show a, Show b) => Show (a `With` b) where
  show (With a b) = show a <> " WITH " <> show b

with :: a -> b -> a `With` b
with = With

withBase :: a `With` b -> a
withBase (a `With` _) = a

findKey :: Ord k => (v -> Bool) -> Map k v -> Maybe k
findKey p = fmap fst . find (p . snd) . mapToList

foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        return $! mappend acc w
    )
    mempty

frequencies :: Ord a => [a] -> Map a Int
frequencies as =
  Map.map getSum $
    foldr (unionWith (<>)) mempty $
      map
        (`Map.singleton` (Sum 1))
        as

groupOnKey :: Ord k => [(k, v)] -> Map k [v]
groupOnKey = Map.fromListWith (++) . map (second pure)

breakM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
breakM _ xs@[] = pure (xs, xs)
breakM p xs@(x : xs') = do
  b <- p x
  if b
    then pure ([], xs)
    else do
      (ys, zs) <- breakM p xs'
      pure (x : ys, zs)

breakNM :: Monad m => Int -> (a -> m Bool) -> [a] -> m ([a], [a])
breakNM n p xs = go n ([], xs)
 where
  go 0 (ys, zs) = pure (reverse ys, zs)
  go _ (ys, []) = pure (reverse ys, [])
  go m (ys, z : zs') = do
    b <- p z
    if b
      then go (m - 1) (z : ys, zs')
      else go m (z : ys, zs')

(<$$>) :: (Functor f, Functor m) => (a -> b) -> m (f a) -> m (f b)
(<$$>) = fmap . fmap
infixl 4 <$$>

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0 ..]

withIndexN :: Int -> [a] -> [(Int, a)]
withIndexN n = zip [n ..]

withIndex1 :: [a] -> [(Int, a)]
withIndex1 = withIndexN 1

newtype Max0 a = Max0 {getMax0 :: a}
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance (Ord a, Num a) => Semigroup (Max0 a) where
  Max0 a <> Max0 b = Max0 $ max 0 (max a b)

instance (Ord a, Num a) => Monoid (Max0 a) where
  mempty = Max0 0

-- since we only use this from `newtype` we just implement it ourselves
ala :: (Coercible a b, Coercible a' b') => (b -> a) -> ((b -> a) -> c -> a') -> c -> b'
ala _ hof = coerce . hof coerce

filterBy :: [a -> Bool] -> [a] -> [a]
filterBy fs = filter (and . sequence fs)

filterByM :: Monad m => [a -> m Bool] -> [a] -> m [a]
filterByM fs = filterM (andM . sequence fs)

foldAllM
  :: (Monoid a, Applicative m, Traversable t, a ~ Element (t a), MonoFoldable (t a)) => t (m a) -> m a
foldAllM xs = fold <$> sequenceA xs

sumAllM
  :: (Element (t (Sum b)) ~ Sum b, Num b, Applicative f, Traversable t, MonoFoldable (t (Sum b)))
  => t (f (Sum b))
  -> f b
sumAllM xs = getSum <$> foldAllM xs

newtype Only a = Only a

type instance Element (Only a) = a

instance MonoFoldable (Only a) where
  otoList (Only a) = [a]

only :: a -> Only a
only = Only

instance Foldable Only where
  foldMap f (Only a) = f a

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM xs f = catMaybes <$> traverse f xs

notNullOr :: [a] -> [a] -> [a]
notNullOr as bs = if null as then bs else as
