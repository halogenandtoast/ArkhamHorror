module Arkham.Types.Card.PlayerCard.Cards.LeoDeLuca where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype LeoDeLuca = LeoDeLuca Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoDeLuca :: CardId -> LeoDeLuca
leoDeLuca cardId = LeoDeLuca $ (asset cardId "01048" "Leo De Luca" 6 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Ally, Criminal]
  }
