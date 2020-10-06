module Arkham.Types.Card.PlayerCard.Cards.BaseballBat where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype BaseballBat = BaseballBat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseballBat :: CardId -> BaseballBat
baseballBat cardId = BaseballBat
  (asset cardId "01074" "Baseball Bat" 2 Survivor)
    { pcSkills = [SkillCombat]
    , pcTraits = [Item, Weapon, Melee]
    }
