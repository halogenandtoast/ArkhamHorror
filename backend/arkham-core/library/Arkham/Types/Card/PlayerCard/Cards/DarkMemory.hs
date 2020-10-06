module Arkham.Types.Card.PlayerCard.Cards.DarkMemory where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype DarkMemory = DarkMemory Attrs
  deriving newtype (Show, ToJSON, FromJSON)

darkMemory :: CardId -> DarkMemory
darkMemory cardId = DarkMemory (event cardId "01013" "Dark Memory" 2 Neutral)
  { pcTraits = [Spell]
  , pcWeakness = True
  }
