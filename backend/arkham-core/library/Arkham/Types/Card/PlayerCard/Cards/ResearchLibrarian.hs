module Arkham.Types.Card.PlayerCard.Cards.ResearchLibrarian where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ResearchLibrarian = ResearchLibrarian Attrs
  deriving newtype (Show, ToJSON, FromJSON)

researchLibrarian :: CardId -> ResearchLibrarian
researchLibrarian cardId =
  ResearchLibrarian $ (asset cardId "01032" "Research Librarian" 2 Seeker)
    { pcSkills = [SkillAgility]
    , pcTraits = [Ally, Miskatonic]
    }

