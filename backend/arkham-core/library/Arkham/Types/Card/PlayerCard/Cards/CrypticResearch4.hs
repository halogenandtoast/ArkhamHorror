module Arkham.Types.Card.PlayerCard.Cards.CrypticResearch4 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait
import Arkham.Types.Window

newtype CrypticResearch4 = CrypticResearch4 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

crypticResearch4 :: CardId -> CrypticResearch4
crypticResearch4 cardId = CrypticResearch4
  (event cardId "01043" "Cryptic Research" 0 Seeker)
    { pcTraits = [Insight]
    , pcLevel = 4
    , pcFast = True
    , pcWindows = setFromList [DuringTurn You]
    }
