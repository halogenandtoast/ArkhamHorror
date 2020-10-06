module Arkham.Types.Card.PlayerCard.Cards.RexsCurse where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype RexsCurse = RexsCurse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rexsCurse :: CardId -> RexsCurse
rexsCurse cardId = RexsCurse $ (treachery cardId "02009" "Rex's Curse" 0)
  { pcTraits = [Curse]
  , pcRevelation = True
  }

