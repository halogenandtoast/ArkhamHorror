module Arkham.Types.Card.PlayerCard.Cards.HotStreak2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype HotStreak2 = HotStreak2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hotStreak2 :: CardId -> HotStreak2
hotStreak2 cardId = HotStreak2 $ (event cardId "50006" "Hot Streak" 5 Rogue)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Fortune]
  , pcLevel = 2
  }
