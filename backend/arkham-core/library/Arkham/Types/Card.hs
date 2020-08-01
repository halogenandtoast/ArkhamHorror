module Arkham.Types.Card
  ( CardCode(..)
  , Card(..)
  , DeckCard(..)
  , PlayerCard(..)
  , EncounterCard(..)
  , PlayerCardType(..)
  , EncounterCardType(..)
  , ClassSymbol(..)
  , HasCardCode(..)
  , HasCardId(..)
  , HasCost(..)
  , HasCard(..)
  , allCards
  , allPlayerCards
  , lookupPlayerCard
  , allEncounterCards
  , encounterCardMatch
  )
where

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import ClassyPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DeckCard = DeckCard { unDeckCard ::PlayerCard }
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

allCards :: HashMap CardCode (CardId -> Card)
allCards =
  HashMap.map (PlayerCard .) allPlayerCards
    <> HashMap.map (EncounterCard .) allEncounterCards
