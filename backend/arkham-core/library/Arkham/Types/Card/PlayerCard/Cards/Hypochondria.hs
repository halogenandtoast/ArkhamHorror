module Arkham.Types.Card.PlayerCard.Cards.Hypochondria where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.Trait

newtype Hypochondria = Hypochondria Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Hypochondria where
  runMessage msg (Hypochondria attrs) = Hypochondria <$> runMessage msg attrs

hypochondria :: CardId -> Hypochondria
hypochondria cardId = Hypochondria $ (treachery cardId "01100" "Hypochondria" 0
                                     )
  { pcTraits = [Madness]
  , pcRevelation = True
  }
