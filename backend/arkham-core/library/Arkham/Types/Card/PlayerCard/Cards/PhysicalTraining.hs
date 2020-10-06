module Arkham.Types.Card.PlayerCard.Cards.PhysicalTraining where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env PhysicalTraining where
  runMessage msg (PhysicalTraining attrs) =
    PhysicalTraining <$> runMessage msg attrs

physicalTraining :: CardId -> PhysicalTraining
physicalTraining cardId =
  PhysicalTraining $ (asset cardId "01017" "Physical Training" 2 Guardian)
    { pcSkills = [SkillWillpower, SkillCombat]
    , pcTraits = [Talent]
    }

