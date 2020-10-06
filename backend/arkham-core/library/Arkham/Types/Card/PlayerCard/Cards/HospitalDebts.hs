module Arkham.Types.Card.PlayerCard.Cards.HospitalDebts where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype HospitalDebts = HospitalDebts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hospitalDebts :: CardId -> HospitalDebts
hospitalDebts cardId =
  HospitalDebts $ (treachery cardId "01011" "Hospital Debts" 0)
    { pcTraits = [Task]
    , pcRevelation = True
    }
