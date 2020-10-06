module Arkham.Types.Card.PlayerCard.Cards.ArcaneStudies where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ArcaneStudies = ArcaneStudies Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies :: CardId -> ArcaneStudies
arcaneStudies cardId = ArcaneStudies
  (asset cardId "01062" "Arcane Studies" 2 Mystic)
    { pcSkills = [SkillWillpower, SkillIntellect]
    , pcTraits = [Talent]
    }
