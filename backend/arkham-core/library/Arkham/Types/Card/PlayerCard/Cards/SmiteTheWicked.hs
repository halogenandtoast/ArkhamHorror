module Arkham.Types.Card.PlayerCard.Cards.SmiteTheWicked where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Trait

newtype SmiteTheWicked = SmiteTheWicked Attrs
  deriving newtype (Show, ToJSON, FromJSON)

smiteTheWicked :: CardId -> SmiteTheWicked
smiteTheWicked cardId =
  SmiteTheWicked $ (treachery cardId "02007" "Smite the Wicked" 0)
    { pcTraits = [Task]
    , pcRevelation = True
    }
