module Arkham.Types.Card.PlayerCard.Cards.Rolands38Special where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Rolands38Special = Rolands38Special Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rolands38Special :: CardId -> Rolands38Special
rolands38Special cardId =
  Rolands38Special $ (asset cardId "01006" "Roland's .38 Special" 3 Neutral)
    { pcSkills = [SkillCombat, SkillAgility, SkillWild]
    , pcTraits = [Item, Weapon, Firearm]
    }
