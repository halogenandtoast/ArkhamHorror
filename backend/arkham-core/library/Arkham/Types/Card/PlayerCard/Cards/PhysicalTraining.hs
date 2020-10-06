module Arkham.Types.Card.PlayerCard.Cards.PhysicalTraining where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining :: CardId -> PhysicalTraining
physicalTraining cardId =
  PhysicalTraining $ (asset cardId "01017" "Physical Training" 2 Guardian)
    { pcSkills = [SkillWillpower, SkillCombat]
    , pcTraits = [Talent]
    }

