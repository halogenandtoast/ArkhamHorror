module Arkham.Types.Card
  ( CardCode(..)
  , Card(..)
  , PlayerCard(..)
  , EncounterCard(..)
  , PlayerCardType(..)
  , EncounterCardType(..)
  , ClassSymbol(..)
  , HasCardCode(..)
  , HasCost(..)
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
import Arkham.Types.Card.PlayerCard
import ClassyPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode Card where
  getCardCode (PlayerCard card) = getCardCode card
  getCardCode (EncounterCard card) = getCardCode card

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost _ = 0

allCards :: HashMap CardCode Card
allCards =
  HashMap.map PlayerCard allPlayerCards
    <> HashMap.map EncounterCard allEncounterCards
