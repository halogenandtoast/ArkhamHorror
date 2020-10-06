module Arkham.Types.Card.PlayerCard.Cards.Hyperawareness where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Hyperawareness = Hyperawareness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness :: CardId -> Hyperawareness
hyperawareness cardId =
  Hyperawareness $ (asset cardId "01034" "Hyperawareness" 2 Seeker)
    { pcSkills = [SkillIntellect, SkillAgility]
    , pcTraits = [Talent]
    }
