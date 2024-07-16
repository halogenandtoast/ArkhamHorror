module Arkham.Helpers where

import Arkham.Prelude hiding (toLower, toUpper, unpack)

import Data.Char (isLetter, toLower, toUpper)
import Data.Data
import Data.Foldable (foldr, foldrM)
import Data.Foldable qualified as Foldable

toLabel :: String -> String
toLabel [] = []
toLabel (x : xs) = toLower x : go xs
 where
  go [] = []
  go (' ' : x' : xs') = toUpper x' : go xs'
  go (x' : xs') = x' : go xs'

replaceNonLetters :: String -> String
replaceNonLetters [] = []
replaceNonLetters (x : xs) =
  if not (isLetter x)
    then case x of
      '\'' -> replaceNonLetters xs
      '.' -> replaceNonLetters xs
      _ -> ' ' : replaceNonLetters xs
    else x : replaceNonLetters xs

foldTokens :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldTokens s tokens f = foldrM (flip f) s tokens

drawCard :: [a] -> (Maybe a, [a])
drawCard [] = (Nothing, [])
drawCard (x : xs) = (Just x, xs)

draw :: forall a. Int -> Deck a -> ([a], Deck a)
draw = coerce (splitAt @[a])

newtype Deck a = Deck {unDeck :: [a]}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Semigroup
    , Monoid
    , ToJSON
    , FromJSON
    , Eq
    , MonoFoldable
    , SemiSequence
    , GrowingAppend
    )

instance Data a => Data (Deck a) where
  gfoldl k z (Deck a) = z Deck `k` a
  gunfold _ _ = error "gunfold"
  toConstr _ = error "toConstr"
  dataTypeOf _ = error "dataTypeOf"

type instance Element (Deck a) = a

instance Show (Deck a) where
  show _ = "<Deck>"

newtype Bag a = Bag {unBag :: [a]}
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Show (Bag a) where
  show _ = "<Bag>"

instance IsSequence (Deck a) where
  fromList = Deck

instance MonoFunctor (Deck a)
instance MonoTraversable (Deck a)
instance MonoPointed (Deck a)

instance Foldable Deck where
  foldr f s (Deck xs) = Foldable.foldr f s xs

instance Traversable Deck where
  traverse f (Deck xs) = Deck <$> traverse f xs
