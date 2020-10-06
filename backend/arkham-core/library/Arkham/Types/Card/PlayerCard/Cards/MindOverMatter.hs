module Arkham.Types.Card.PlayerCard.Cards.MindOverMatter where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MindOverMatter = MindOverMatter Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindOverMatter :: CardId -> MindOverMatter
mindOverMatter cardId =
  MindOverMatter $ (event cardId "01036" "Mind over Matter" 1 Seeker)
    { pcSkills = [SkillCombat, SkillAgility]
    , pcTraits = [Insight]
    , pcFast = True
    , pcWindows = setFromList [DuringTurn You]
    }
