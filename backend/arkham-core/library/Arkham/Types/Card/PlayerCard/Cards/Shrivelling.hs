module Arkham.Types.Card.PlayerCard.Cards.Shrivelling where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Shrivelling = Shrivelling Attrs
  deriving newtype (Show, ToJSON, FromJSON)

shrivelling :: CardId -> Shrivelling
shrivelling cardId = Shrivelling $ (asset cardId "01060" "Shrivelling" 3 Mystic
                                   )
  { pcSkills = [SkillCombat]
  , pcTraits = [Spell]
  }
