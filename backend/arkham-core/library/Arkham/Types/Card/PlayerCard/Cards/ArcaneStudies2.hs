module Arkham.Types.Card.PlayerCard.Cards.ArcaneStudies2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ArcaneStudies2 = ArcaneStudies2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies2 :: CardId -> ArcaneStudies2
arcaneStudies2 cardId = ArcaneStudies2
  (asset cardId "50007" "Arcane Studies" 0 Mystic)
    { pcSkills =
      [SkillWillpower, SkillWillpower, SkillIntellect, SkillIntellect]
    , pcTraits = [Talent]
    , pcLevel = 2
    }
