module Arkham.Types.Card.CardMatcher where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardType
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

data CardMatcher
  = CardMatchByType CardType
  | CardMatchByCardCode CardCode
  | CardMatchByTitle Text
  | CardMatchByTrait Trait
  | CardMatchByClass ClassSymbol
  | CardMatchByOneOf [CardMatcher]
  | CardMatchers [CardMatcher]
  | AnyCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatchers xs <> CardMatchers ys = CardMatchers $ xs <> ys
  CardMatchers xs <> x = CardMatchers (x : xs)
  x <> CardMatchers xs = CardMatchers (x : xs)
  x <> y = CardMatchers [x, y]
