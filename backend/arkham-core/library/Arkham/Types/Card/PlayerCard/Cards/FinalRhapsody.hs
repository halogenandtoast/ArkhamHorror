module Arkham.Types.Card.PlayerCard.Cards.FinalRhapsody where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype FinalRhapsody = FinalRhapsody Attrs
  deriving newtype (Show, ToJSON, FromJSON)

finalRhapsody :: CardId -> FinalRhapsody
finalRhapsody cardId = FinalRhapsody
  (treachery cardId "02013" "Final Rhapsody" 0)
    { pcTraits = [Endtimes]
    , pcRevelation = True
    }
