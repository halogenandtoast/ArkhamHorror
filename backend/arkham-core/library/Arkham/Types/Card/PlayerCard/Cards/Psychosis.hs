module Arkham.Types.Card.PlayerCard.Cards.Psychosis where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype Psychosis = Psychosis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

psychosis :: CardId -> Psychosis
psychosis cardId = Psychosis $ (treachery cardId "01099" "Psychosis" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

