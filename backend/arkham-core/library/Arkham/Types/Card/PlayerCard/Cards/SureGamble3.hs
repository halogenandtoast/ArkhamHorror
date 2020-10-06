module Arkham.Types.Card.PlayerCard.Cards.SureGamble3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait
import Arkham.Types.Window

newtype SureGamble3 = SureGamble3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env SureGamble3 where
  runMessage msg (SureGamble3 attrs) = SureGamble3 <$> runMessage msg attrs

sureGamble3 :: CardId -> SureGamble3
sureGamble3 cardId = SureGamble3 $ (asset cardId "01056" "Sure Gamble" 2 Rogue)
  { pcTraits = [Fortune, Insight]
  , pcFast = True
  , pcWindows = setFromList [WhenRevealTokenWithNegativeModifier You]
  , pcLevel = 3
  }
