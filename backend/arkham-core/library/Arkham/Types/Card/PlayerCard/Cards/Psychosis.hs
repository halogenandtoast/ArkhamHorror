module Arkham.Types.Card.PlayerCard.Cards.Psychosis where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.Trait

newtype Psychosis = Psychosis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Psychosis where
  runMessage msg (Psychosis attrs) = Psychosis <$> runMessage msg attrs

psychosis :: CardId -> Psychosis
psychosis cardId = Psychosis $ (treachery cardId "01099" "Psychosis" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

