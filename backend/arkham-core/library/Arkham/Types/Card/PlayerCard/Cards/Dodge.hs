module Arkham.Types.Card.PlayerCard.Cards.Dodge where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Dodge = Dodge Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Dodge where
  runMessage msg (Dodge attrs) = Dodge <$> runMessage msg attrs

dodge :: CardId -> Dodge
dodge cardId = Dodge (event cardId "01023" "Dodge" 1 Guardian)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = [Tactic]
  , pcFast = True
  , pcWindows = setFromList [WhenEnemyAttacks InvestigatorAtYourLocation]
  }
