module Arkham.Types.Card.PlayerCard.Cards.Machete where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Machete = Machete Attrs
  deriving newtype (Show, ToJSON, FromJSON)

machete :: CardId -> Machete
machete cardId = Machete $ (asset cardId "01020" "Machete" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }
