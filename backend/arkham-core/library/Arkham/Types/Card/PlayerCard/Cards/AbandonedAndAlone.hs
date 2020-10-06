module Arkham.Types.Card.PlayerCard.Cards.AbandonedAndAlone where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype AbandonedAndAlone = AbandonedAndAlone Attrs
  deriving newtype (Show, ToJSON, FromJSON)

abandonedAndAlone :: CardId -> AbandonedAndAlone
abandonedAndAlone cardId = AbandonedAndAlone
  (treachery cardId "01015" "Abandoned and Alone" 0)
    { pcTraits = [Madness]
    , pcRevelation = True
    }
