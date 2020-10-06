module Arkham.Types.Card.PlayerCard.Cards.PhysicalTraining2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype PhysicalTraining2 = PhysicalTraining2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining2 :: CardId -> PhysicalTraining2
physicalTraining2 cardId =
  PhysicalTraining2 $ (asset cardId "50001" "Physical Training" 0 Guardian)
    { pcSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillCombat]
    , pcTraits = [Talent]
    , pcLevel = 2
    }
