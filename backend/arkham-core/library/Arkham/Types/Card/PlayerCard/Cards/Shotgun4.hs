module Arkham.Types.Card.PlayerCard.Cards.Shotgun4 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Shotgun4 = Shotgun4 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

shotgun4 :: CardId -> Shotgun4
shotgun4 cardId = Shotgun4 $ (asset cardId "01029" "Shotgun" 5 Guardian)
  { pcSkills = [SkillCombat, SkillCombat]
  , pcTraits = [Item, Weapon, Firearm]
  , pcLevel = 4
  }
