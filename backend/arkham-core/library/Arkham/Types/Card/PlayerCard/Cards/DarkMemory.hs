module Arkham.Types.Card.PlayerCard.Cards.DarkMemory where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype DarkMemory = DarkMemory Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env DarkMemory where
  runMessage msg (DarkMemory attrs) = DarkMemory <$> runMessage msg attrs

darkMemory :: CardId -> DarkMemory
darkMemory cardId = DarkMemory (event cardId "01013" "Dark Memory" 2 Neutral)
  { pcTraits = [Spell]
  , pcWeakness = True
  }
