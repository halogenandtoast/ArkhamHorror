module Arkham.Types.Card.PlayerCard.Cards.Barricade3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Barricade3 = Barricade3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

barricade3 :: CardId -> Barricade3
barricade3 cardId = Barricade3 (event cardId "50004" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = [Insight, Tactic]
  , pcLevel = 3
  }
