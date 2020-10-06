module Arkham.Types.Card.PlayerCard.Cards.LitaChantler where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype LitaChantler = LitaChantler Attrs
  deriving newtype (Show, ToJSON, FromJSON)

litaChantler :: CardId -> LitaChantler
litaChantler cardId = LitaChantler
  (asset cardId "01117" "Lita Chantler" 0 Neutral) { pcTraits = [Ally] }
