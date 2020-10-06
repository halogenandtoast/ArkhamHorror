module Arkham.Types.Card.PlayerCard.Cards.HardKnocks where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype HardKnocks = HardKnocks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks :: CardId -> HardKnocks
hardKnocks cardId = HardKnocks (asset cardId "01049" "Hard Knocks" 2 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Talent]
  }
