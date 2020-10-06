module Arkham.Types.Card.PlayerCard.Cards.PoliceBadge2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype PoliceBadge2 = PoliceBadge2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

policeBadge2 :: CardId -> PoliceBadge2
policeBadge2 cardId =
  PoliceBadge2 $ (asset cardId "01027" "Police Badge" 3 Guardian)
    { pcSkills = [SkillWillpower, SkillWild]
    , pcTraits = [Item]
    , pcLevel = 2
    }
