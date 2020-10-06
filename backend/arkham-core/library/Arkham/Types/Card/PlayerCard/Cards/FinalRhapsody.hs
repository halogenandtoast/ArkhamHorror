module Arkham.Types.Card.PlayerCard.Cards.FinalRhapsody where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.Trait

newtype FinalRhapsody = FinalRhapsody Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env FinalRhapsody where
  runMessage msg (FinalRhapsody attrs) = FinalRhapsody <$> runMessage msg attrs

finalRhapsody :: CardId -> FinalRhapsody
finalRhapsody cardId = FinalRhapsody
  (treachery cardId "02013" "Final Rhapsody" 0)
    { pcTraits = [Endtimes]
    , pcRevelation = True
    }
