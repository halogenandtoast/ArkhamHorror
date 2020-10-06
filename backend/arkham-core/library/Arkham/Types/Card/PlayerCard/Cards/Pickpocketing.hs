module Arkham.Types.Card.PlayerCard.Cards.Pickpocketing where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Pickpocketing = Pickpocketing Attrs
  deriving newtype (Show, ToJSON, FromJSON)

pickpocketing :: CardId -> Pickpocketing
pickpocketing cardId =
  Pickpocketing $ (asset cardId "01046" "Pickpocketing" 2 Rogue)
    { pcSkills = [SkillAgility]
    , pcTraits = [Talent, Illicit]
    }
