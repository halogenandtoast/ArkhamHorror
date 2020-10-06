module Arkham.Types.Card.PlayerCard.Cards.Encyclopedia2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Encyclopedia2 = Encyclopedia2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

encyclopedia2 :: CardId -> Encyclopedia2
encyclopedia2 cardId = Encyclopedia2
  (asset cardId "01042" "Encyclopedia" 2 Seeker)
    { pcSkills = [SkillWild]
    , pcTraits = [Item, Tome]
    , pcLevel = 2
    }
