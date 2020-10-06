module Arkham.Types.Card.PlayerCard.Cards.Amnesia where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype Amnesia = Amnesia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

amnesia :: CardId -> Amnesia
amnesia cardId = Amnesia (treachery cardId "01096" "Amnesia" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }
