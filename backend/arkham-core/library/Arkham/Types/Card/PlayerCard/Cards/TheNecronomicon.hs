module Arkham.Types.Card.PlayerCard.Cards.TheNecronomicon where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype TheNecronomicon = TheNecronomicon Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theNecronomicon :: CardId -> TheNecronomicon
theNecronomicon cardId =
  TheNecronomicon $ (asset cardId "01009" "The Necronomicon" 0 Neutral)
    { pcTraits = [Item, Tome]
    , pcWeakness = True
    , pcRevelation = True
    }
