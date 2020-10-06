module Arkham.Types.Card.PlayerCard.Cards.CloseCall2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype CloseCall2 = CloseCall2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env CloseCall2 where
  runMessage msg (CloseCall2 attrs) = CloseCall2 <$> runMessage msg attrs

closeCall2 :: CardId -> CloseCall2
closeCall2 cardId = CloseCall2 (event cardId "01083" "Close Call" 2 Survivor)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Fortune]
  , pcFast = True
  , pcWindows = setFromList [AfterEnemyEvaded You (error "does not work")]
  }
