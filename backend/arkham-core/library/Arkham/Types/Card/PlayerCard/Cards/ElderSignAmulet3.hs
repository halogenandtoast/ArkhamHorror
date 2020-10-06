module Arkham.Types.Card.PlayerCard.Cards.ElderSignAmulet3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ElderSignAmulet3 = ElderSignAmulet3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

elderSignAmulet3 :: CardId -> ElderSignAmulet3
elderSignAmulet3 cardId = ElderSignAmulet3
  (asset cardId "01095" "Elder Sign Amulet" 2 Neutral)
    { pcSkills = [SkillWillpower, SkillWild]
    , pcTraits = [Item, Relic]
    , pcLevel = 3
    }
