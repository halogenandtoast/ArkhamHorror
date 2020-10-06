module Arkham.Types.Card.PlayerCard.Cards.ZoeysCross where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ZoeysCross = ZoeysCross Attrs
  deriving newtype (Show, ToJSON, FromJSON)

zoeysCross :: CardId -> ZoeysCross
zoeysCross cardId = ZoeysCross $ (asset cardId "02006" "Zoey's Cross" 1 Neutral
                                 )
  { pcSkills = [SkillCombat, SkillCombat, SkillWild]
  , pcTraits = [Item, Charm]
  }
