module Arkham.Types.Card.PlayerCard.Cards.HotStreak4 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype HotStreak4 = HotStreak4 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hotStreak4 :: CardId -> HotStreak4
hotStreak4 cardId = HotStreak4 $ (event cardId "01057" "Hot Streak" 2 Rogue)
  { pcSkills = [SkillWild]
  , pcTraits = [Fortune]
  }
