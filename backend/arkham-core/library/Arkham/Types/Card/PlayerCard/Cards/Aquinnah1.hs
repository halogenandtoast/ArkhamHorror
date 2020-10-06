module Arkham.Types.Card.PlayerCard.Cards.Aquinnah1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Aquinnah1 = Aquinnah1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

aquinnah1 :: CardId -> Aquinnah1
aquinnah1 cardId = Aquinnah1 (asset cardId "01082" "Aquinnah" 5 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Ally]
  }
