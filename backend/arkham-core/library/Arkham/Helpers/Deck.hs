module Arkham.Helpers.Deck where

import Arkham.Prelude

import Arkham.Helpers
import Arkham.Card.CardDef

withDeck :: ([a] -> [a]) -> Deck a -> Deck a
withDeck f (Deck xs) = Deck (f xs)

withDeckM :: Functor f => ([a] -> f [a]) -> Deck a -> f (Deck a)
withDeckM f (Deck xs) = Deck <$> f xs

removeEachFromDeck :: HasCardDef a => Deck a -> [CardDef] -> Deck a
removeEachFromDeck deck removals = flip withDeck deck $ \cards ->
  foldl' (\cs m -> deleteFirstMatch ((== m) . toCardDef) cs) cards removals
