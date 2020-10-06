module Arkham.Types.Card.PlayerCard.Cards.Flashlight where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Flashlight = Flashlight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

flashlight :: CardId -> Flashlight
flashlight cardId = Flashlight (asset cardId "01087" "Flashlight" 2 Neutral)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Item, Tool]
  }
