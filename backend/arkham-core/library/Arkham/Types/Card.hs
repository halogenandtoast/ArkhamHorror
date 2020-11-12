module Arkham.Types.Card
  ( CardCode(..)
  , Card(..)
  , DiscardedPlayerCard(..)
  , DeckCard(..)
  , HandCard(..)
  , DiscardableHandCard(..)
  , InPlayCard(..)
  , PlayerCard(..)
  , EncounterCard(..)
  , PlayerCardType(..)
  , EncounterCardType(..)
  , HasCardCode(..)
  , HasCardId(..)
  , HasCost(..)
  , HasCard(..)
  , BearerId(..)
  , AttackOfOpportunityModifier(..)
  , allPlayerCards
  , lookupCard
  , lookupPlayerCard
  , genPlayerCard
  , lookupEncounterCard
  , allEncounterCards
  , encounterCardMatch
  , playerCardMatch
  , toPlayerCard
  , toEncounterCard
  , cardIsWeakness
  , isDynamic
  )
where

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Cost
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import ClassyPrelude
import Data.Aeson

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DeckCard = DeckCard { unDeckCard ::PlayerCard }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype HandCard = HandCard { unHandCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype DiscardableHandCard = DiscardableHandCard { unDiscardableHandCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype InPlayCard = InPlayCard { unInPlayCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

class HasCard b a where
  getCard :: b -> CardId -> a -> Card

instance HasCardCode Card where
  getCardCode (PlayerCard card) = getCardCode card
  getCardCode (EncounterCard card) = getCardCode card

instance HasCardId Card where
  getCardId (PlayerCard card) = getCardId card
  getCardId (EncounterCard card) = getCardId card

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost _ = 0

isDynamic :: Card -> Bool
isDynamic (PlayerCard card) = case pcCost card of
  DynamicCost -> True
  _ -> False
isDynamic _ = False

toPlayerCard :: Card -> Maybe PlayerCard
toPlayerCard (PlayerCard pc) = Just pc
toPlayerCard _ = Nothing

toEncounterCard :: Card -> Maybe EncounterCard
toEncounterCard (EncounterCard ec) = Just ec
toEncounterCard _ = Nothing

cardIsWeakness :: Card -> Bool
cardIsWeakness (EncounterCard _) = False
cardIsWeakness (PlayerCard pc) = pcWeakness pc

lookupCard :: CardCode -> Maybe (CardId -> Card)
lookupCard cardCode =
  let
    encounterCard = do
      f <- lookup cardCode allEncounterCards
      pure $ EncounterCard . f
    playerCard = do
      f <- lookup cardCode allPlayerCards
      pure $ PlayerCard . f
  in encounterCard <|> playerCard
