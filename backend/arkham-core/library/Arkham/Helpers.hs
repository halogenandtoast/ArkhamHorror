module Arkham.Helpers where

import Arkham.Prelude hiding (toLower, toUpper, unpack)

import Data.Char (isLetter, toLower, toUpper)
import Data.Foldable (foldrM)

toLabel :: String -> String
toLabel [] = []
toLabel (x : xs) = toLower x : go xs
 where
  go [] = []
  go (' ' : x' : xs') = toUpper x' : go xs'
  go (x' : xs') = x' : go xs'

replaceNonLetters :: String -> String
replaceNonLetters [] = []
replaceNonLetters (x : xs) = if not (isLetter x)
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

newtype Deck a = Deck { unDeck :: [a] }
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, Eq)

withDeck :: ([a] -> [a]) -> Deck a -> Deck a
withDeck f (Deck xs) = Deck (f xs)

instance Show (Deck a) where
  show _ = "<Deck>"

newtype Bag a = Bag { unBag :: [a] }
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Show (Bag a) where
  show _ = "<Bag>"
