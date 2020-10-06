module Arkham.Types.Card.PlayerCard.Cards.ForbiddenKnowledge where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ForbiddenKnowledge = ForbiddenKnowledge Attrs
  deriving newtype (Show, ToJSON, FromJSON)

forbiddenKnowledge :: CardId -> ForbiddenKnowledge
forbiddenKnowledge cardId = ForbiddenKnowledge
  (asset cardId "01058" "Forbidden Knowledge" 0 Mystic)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Talent]
    }
