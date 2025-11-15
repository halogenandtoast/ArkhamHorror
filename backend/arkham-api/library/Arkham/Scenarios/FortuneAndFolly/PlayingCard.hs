{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Scenarios.FortuneAndFolly.PlayingCard where

import Arkham.Card.CardDef
import Arkham.Prelude
import Data.Aeson.TH
import GHC.Records

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Rank = Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data PlayingCard = PlayingCard
  { rank :: Rank
  , suit :: Suit
  }
  deriving stock (Ord, Eq, Show)

toPlayingCardPure :: HasCardDef a => a -> Maybe PlayingCard
toPlayingCardPure a = PlayingCard <$> rank <*> suit
 where
  cardDef = toCardDef a
  suit = toSuit =<< lookup "suit" (cdMeta cardDef)
  toSuit = \case
    "hearts" -> Just Hearts
    "diamonds" -> Just Diamonds
    "clubs" -> Just Clubs
    "spades" -> Just Spades
    _ -> Nothing
  rank = toRank =<< lookup "value" (cdMeta cardDef)
  toRank = \case
    "four" -> Just Four
    "five" -> Just Five
    "six" -> Just Six
    "seven" -> Just Seven
    "eight" -> Just Eight
    "nine" -> Just Nine
    "ten" -> Just Ten
    "jack" -> Just Jack
    "queen" -> Just Queen
    "king" -> Just King
    "ace" -> Just Ace
    _ -> Nothing

rankValue :: PlayingCard -> Int
rankValue pc = case pc.rank of
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 11
  Queen -> 12
  King -> 13
  Ace -> 14

numericValue :: PlayingCard -> Int
numericValue pc = case pc.rank of
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 10
  Queen -> 10
  King -> 10
  Ace -> 10

instance HasField "value" PlayingCard Int where
  getField = numericValue

foldMap (deriveJSON defaultOptions) [''Rank, ''Suit, ''PlayingCard]
