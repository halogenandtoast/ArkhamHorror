module Arkham.Types.Card.PlayerCard.Cards.Haunted where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.Trait

newtype Haunted = Haunted Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Haunted where
  runMessage msg (Haunted attrs) = Haunted <$> runMessage msg attrs

haunted :: CardId -> Haunted
haunted cardId = Haunted $ (treachery cardId "01098" "Haunted" 0)
  { pcTraits = [Curse]
  , pcRevelation = True
  }
