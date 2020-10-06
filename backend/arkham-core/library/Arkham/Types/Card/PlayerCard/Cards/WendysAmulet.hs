module Arkham.Types.Card.PlayerCard.Cards.WendysAmulet where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype WendysAmulet = WendysAmulet Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs

wendysAmulet :: CardId -> WendysAmulet
wendysAmulet cardId =
  WendysAmulet $ (asset cardId "01014" "Wendy's Amulet" 2 Neutral)
    { pcSkills = [SkillWild, SkillWild]
    , pcTraits = [Item, Relic]
    }
