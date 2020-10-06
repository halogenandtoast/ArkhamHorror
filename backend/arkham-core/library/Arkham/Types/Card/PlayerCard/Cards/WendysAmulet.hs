module Arkham.Types.Card.PlayerCard.Cards.WendysAmulet where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype WendysAmulet = WendysAmulet Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendysAmulet :: CardId -> WendysAmulet
wendysAmulet cardId =
  WendysAmulet $ (asset cardId "01014" "Wendy's Amulet" 2 Neutral)
    { pcSkills = [SkillWild, SkillWild]
    , pcTraits = [Item, Relic]
    }
