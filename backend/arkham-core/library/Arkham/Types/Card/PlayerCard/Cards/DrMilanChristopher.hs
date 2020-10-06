module Arkham.Types.Card.PlayerCard.Cards.DrMilanChristopher where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DrMilanChristopher = DrMilanChristopher Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drMilanChristopher :: CardId -> DrMilanChristopher
drMilanChristopher cardId = DrMilanChristopher
  (asset cardId "01033" "Dr. Milan Christopher" 4 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Ally, Miskatonic]
    }
