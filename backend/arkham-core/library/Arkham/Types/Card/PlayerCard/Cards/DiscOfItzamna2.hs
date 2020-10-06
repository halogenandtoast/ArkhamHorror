module Arkham.Types.Card.PlayerCard.Cards.DiscOfItzamna2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DiscOfItzamna2 = DiscOfItzamna2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

discOfItzamna2 :: CardId -> DiscOfItzamna2
discOfItzamna2 cardId = DiscOfItzamna2
  (asset cardId "01041" "Disc of Itzamna" 3 Seeker)
    { pcSkills = [SkillWillpower, SkillIntellect, SkillCombat]
    , pcTraits = [Item, Relic]
    , pcLevel = 2
    }
