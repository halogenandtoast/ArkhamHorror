module Arkham.Types.Card.PlayerCard.Cards.TheNecronomicon where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype TheNecronomicon = TheNecronomicon Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env TheNecronomicon where
  runMessage msg (TheNecronomicon attrs) =
    TheNecronomicon <$> runMessage msg attrs

theNecronomicon :: CardId -> TheNecronomicon
theNecronomicon cardId =
  TheNecronomicon $ (asset cardId "01009" "The Necronomicon" 0 Neutral)
    { pcTraits = [Item, Tome]
    , pcWeakness = True
    , pcRevelation = True
    }
