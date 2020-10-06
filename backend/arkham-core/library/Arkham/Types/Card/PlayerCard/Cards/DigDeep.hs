module Arkham.Types.Card.PlayerCard.Cards.DigDeep where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DigDeep = DigDeep Attrs
  deriving newtype (Show, ToJSON, FromJSON)

digDeep :: CardId -> DigDeep
digDeep cardId = DigDeep (asset cardId "01077" "Dig Deep" 2 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Talent]
  }
