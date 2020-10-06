module Arkham.Types.Card.PlayerCard.Cards.HardKnocks2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype HardKnocks2 = HardKnocks2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks2 :: CardId -> HardKnocks2
hardKnocks2 cardId = HardKnocks2 (asset cardId "50005" "Hard Knocks" 0 Rogue)
  { pcSkills = [SkillCombat, SkillCombat, SkillAgility, SkillAgility]
  , pcTraits = [Talent]
  , pcLevel = 2
  }
