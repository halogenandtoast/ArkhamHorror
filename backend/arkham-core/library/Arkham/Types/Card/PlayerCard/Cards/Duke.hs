module Arkham.Types.Card.PlayerCard.Cards.Duke where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype Duke = Duke Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Duke where
  runMessage msg (Duke attrs) = Duke <$> runMessage msg attrs

duke :: CardId -> Duke
duke cardId =
  Duke (asset cardId "02014" "Duke" 2 Neutral) { pcTraits = [Ally, Creature] }
