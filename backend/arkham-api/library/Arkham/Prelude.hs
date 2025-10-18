module Arkham.Prelude (
  module X,
  module Arkham.Prelude,
) where

import ClassyPrelude as X hiding (foldlM, on, (\\))
import Data.Type.Equality as X (type (~))

import Control.Exception as X (throw)
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
  zipWithM_,
 )
import Control.Monad.Random as X (MonadRandom, uniform)
import Control.Monad.Random.Class as X (getRandom, getRandomR, getRandoms)
import Control.Monad.Random.Strict as X (Random)
import Control.Monad.Trans.Maybe as X (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Aeson (Result (..))
import Data.Aeson as X hiding (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text
import Data.Align as X (align)
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
import Data.These as X (These (..))
import Data.UUID as X (UUID)
import GHC.Stack as X
import Language.Haskell.TH hiding (location)
import Safe as X (fromJustNote)
import System.Random.Shuffle as X hiding (shuffle)

import Control.Monad.Trans.Class
import Data.Aeson.Key qualified as Key
import Data.Foldable (Foldable (foldMap), foldlM)
import Data.List.NonEmpty qualified as NE
import Data.Proxy
import GHC.TypeLits

class Not a where
  not_ :: a -> a

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

lNamer :: FieldNamer
lNamer _ _ n = [TopName (mkName (nameBase n ++ "L"))]

lFields :: LensRules
lFields = defaultFieldRules & lensField .~ lNamer

guardM :: (Alternative m, Monad m) => m Bool -> m ()
guardM p = p >>= guard

ensure :: (Alternative (t m), Monad m, MonadTrans t) => m Bool -> t m ()
ensure = liftGuardM

liftGuardM :: (Alternative (t m), Monad m, MonadTrans t) => m Bool -> t m ()
liftGuardM p = lift p >>= guard

ensureAll :: (MonadTrans t, Monad m, Alternative (t m)) => [m Bool] -> t m ()
ensureAll = liftGuardsM

liftGuardsM :: (MonadTrans t, Monad m, Alternative (t m)) => [m Bool] -> t m ()
liftGuardsM as = foldr ((>>) . liftGuardM) (pure ()) as

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

sampleNonEmptyN :: (Eq a, MonadRandom m) => Int -> NonEmpty a -> m (NonEmpty a)
sampleNonEmptyN 0 _ = error "Must be value > 0"
sampleNonEmptyN _ (x :| []) = pure $ x :| []
sampleNonEmptyN n xs = do
  (x, rest) <- sampleWithRest xs
  case nonEmpty rest of
    Nothing -> pure (x :| [])
    Just xs' -> (x :|) <$> sampleN (n - 1) xs'

sampleListN :: (Eq (Element mono), MonoFoldable mono, MonadRandom m) => Int -> mono -> m [Element mono]
sampleListN n xs = maybe (pure []) (sampleN n) (nonEmpty $ otoList xs)

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

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 z ~(a, b, c, d, e) = z a b c d e

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 z ~(a, b, c, d, e, f, g) = z a b c d e f g

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

data Envelope (sym :: Symbol) a = Envelope a
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Data, Generic)

instance (ToJSON a, KnownSymbol sym) => ToJSON (Envelope sym a) where
  toJSON (Envelope a) = object [Key.fromString (symbolVal (Proxy @sym)) .= a]

instance (FromJSON a, KnownSymbol sym) => FromJSON (Envelope sym a) where
  parseJSON = withObject "Envelope" $ \o -> Envelope <$> o .: Key.fromString (symbolVal (Proxy @sym))

instance Ord a => Ord (Envelope sym a) where
  compare (Envelope a1) (Envelope a2) = compare a1 a2

data With a b = With a b
  deriving stock Data

instance (Ord a, Ord b) => Ord (With a b) where
  With a1 b1 `compare` With a2 b2 = (a1, b1) `compare` (a2, b2)

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
        pure $! mappend acc w
    )
    mempty

frequencies :: Ord a => [a] -> Map a Int
frequencies as = Map.map getSum $ foldr (unionWith (<>) . (`Map.singleton` Sum 1)) mempty as

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
(<$$>) = ffmap
infixl 4 <$$>

(<|?>) :: [a] -> [a] -> [a]
xs <|?> ys | null xs   = ys
           | otherwise = xs
infixl 3 <|?>

ffmap :: (Functor f, Functor m) => (a -> b) -> m (f a) -> m (f b)
ffmap = fmap . fmap

withIndex :: MonoFoldable l => l -> [(Int, Element l)]
withIndex = zip [0 ..] . toList

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

filterMapM :: (Ord k, Monad m) => (v -> m Bool) -> Map k v -> m (Map k v)
filterMapM f = fmap mapFromList . filterM (f . snd) . mapToList

filterBy :: [a -> Bool] -> [a] -> [a]
filterBy fs = filter (and . sequence fs)

filterByM :: Monad m => [a -> m Bool] -> [a] -> m [a]
filterByM fs = filterM (andM . sequence fs)

partitionByM :: Monad m => [a -> m Bool] -> [a] -> m ([a], [a])
partitionByM fs = partitionM (andM . sequence fs)

foldAllM
  :: (Monoid a, Applicative m, Traversable t, a ~ Element (t a), MonoFoldable (t a)) => t (m a) -> m a
foldAllM xs = fold <$> sequenceA xs

sumM
  :: (Num b, Applicative m, Traversable t, b ~ Element (t b), MonoFoldable (t b))
  => (a -> m b) -> t a -> m b
sumM f xs = sum <$> traverse f xs

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

each_ :: (Monad m, MonoFoldable as) => m as -> (Element as -> m ()) -> m ()
each_ as f = traverse_ f =<< as

upon :: Applicative m => m () -> Bool -> m ()
upon = flip when

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing ma body = when (isNothing ma) body

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mma f = mma >>= traverse_ f

concatMapMaybe :: (a -> Maybe [b]) -> [a] -> [b]
concatMapMaybe f = concat . mapMaybe f

notNullOr :: [a] -> [a] -> [a]
notNullOr as bs = if null as then bs else as

toSentence :: Show a => [a] -> Text
toSentence = go False
 where
  go _ [] = ""
  go _ [a] = tshow a
  go True [a, b] = tshow a <> ", and " <> tshow b
  go False [a, b] = tshow a <> " and " <> tshow b
  go _ (a : as) = tshow a <> ", " <> go True as

toResult :: (HasCallStack, FromJSON a) => Value -> a
toResult x = case fromJSON x of
  Success a -> a
  Error e -> error $ "result failure: " <> e

toResultDefault :: FromJSON a => a -> Value -> a
toResultDefault def x = case fromJSON x of
  Success a -> a
  Error _ -> def

maybeResult :: FromJSON a => Value -> Maybe a
maybeResult x = case fromJSON x of
  Success a -> Just a
  Error _ -> Nothing

countOccurrences :: Ord a => [a] -> Map a Int
countOccurrences = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

-- Function to find all elements of the first list that appear the fewest number of times in the second list.
findFewestOccurrences :: Ord a => [a] -> [a] -> [a]
findFewestOccurrences uniqueTargets as
  | notInTargets /= [] = notInTargets
  | otherwise = filter (\t -> Map.findWithDefault 0 t targetCounts == minOccurrence) uniqueTargets
 where
  targetCounts = countOccurrences as
  notInTargets = filter (`notElem` uniqueTargets) as
  minOccurrence = maybe 0 snd (Map.lookupMin (Map.fromListWith min (Map.toList targetCounts)))

shuffle :: MonadRandom m => [a] -> m [a]
shuffle = shuffleM

shuffleIn :: MonadRandom m => a -> [a] -> m [a]
shuffleIn x xs = shuffleM (x : xs)

removeRandom :: (MonadRandom m, Eq a) => [a] -> m [a]
removeRandom [] = pure []
removeRandom (x : xs) = snd <$> sampleWithRest (x :| xs)

ifM_ :: Monad m => m Bool -> a -> a -> m a
ifM_ body tVal fVal = do
  cond <- body
  pure $ if cond then tVal else fVal

ifMM_ :: Monad m => m Bool -> m a -> m a -> m a
ifMM_ body tVal fVal = do
  cond <- body
  if cond then tVal else fVal

given :: (Monad m, Monoid a) => m a -> Bool -> m a
given ma b = if b then ma else pure mempty

guarded :: (Monad m, Monoid a) => Bool -> m a -> m a
guarded = flip given

class ToDisplay a where
  toDisplay :: a -> Text

mapFold :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
mapFold = flip Data.Foldable.foldMap

runDefaultMaybeT :: Functor f => b -> MaybeT f b -> f b
runDefaultMaybeT def = fmap (fromMaybe def) . runMaybeT

runValidT :: Functor f => MaybeT f () -> f Bool
runValidT = fmap isJust . runMaybeT

runMaybeT_ :: Functor f => MaybeT f () -> f ()
runMaybeT_ = void . runMaybeT

retryUntil :: Monad m => (a -> Bool) -> m a -> m a
retryUntil p ma = do
  a <- ma
  if p a
    then pure a
    else retryUntil p ma
