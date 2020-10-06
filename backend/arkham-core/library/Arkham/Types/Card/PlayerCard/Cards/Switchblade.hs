module Arkham.Types.Card.PlayerCard.Cards.Switchblade where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Switchblade = Switchblade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

switchblade :: CardId -> Switchblade
switchblade cardId = Switchblade (asset cardId "01044" "Switchblade" 1 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = [Item, Weapon, Melee, Illicit]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }
