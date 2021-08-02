{-# LANGUAGE PatternSynonyms #-}
module Arkham.Types.Card.CardMatcher where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardType
import Arkham.Types.ClassSymbol
import Arkham.Types.Keyword
import Arkham.Types.Matcher
import Arkham.Types.Trait

pattern NonWeaknessTreachery :: CardMatcher
pattern NonWeaknessTreachery =
  CardMatches [NonWeakness, CardWithType TreacheryType]

pattern NonPeril :: CardMatcher
pattern NonPeril <- CardWithoutKeyword Peril where
  NonPeril = CardWithoutKeyword Peril

pattern EventCard :: CardMatcher
pattern EventCard <- CardWithType EventType where
  EventCard = CardWithType EventType

pattern AssetCard :: CardMatcher
pattern AssetCard <- CardWithType AssetType where
  AssetCard = CardWithType AssetType

data CardMatcher
  = CardWithType CardType
  | CardIsBeneathInvestigator Who
  | CardWithCardCode CardCode
  | CardWithTitle Text
  | CardWithTrait Trait
  | CardWithoutKeyword Keyword
  | CardWithClass ClassSymbol
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | InHandOf Who
  | IsPlayable
  | NonWeakness
  | NonExceptional
  | AnyCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatches xs <> CardMatches ys = CardMatches $ xs <> ys
  CardMatches xs <> x = CardMatches (x : xs)
  x <> CardMatches xs = CardMatches (x : xs)
  x <> y = CardMatches [x, y]
