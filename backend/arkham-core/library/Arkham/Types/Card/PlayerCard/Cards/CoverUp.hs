module Arkham.Types.Card.PlayerCard.Cards.CoverUp where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.Trait

newtype CoverUp = CoverUp Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env CoverUp where
  runMessage msg (CoverUp attrs) = CoverUp <$> runMessage msg attrs

coverUp :: CardId -> CoverUp
coverUp cardId = CoverUp (treachery cardId "01007" "Cover Up" 0)
  { pcTraits = [Task]
  , pcRevelation = True
  }
