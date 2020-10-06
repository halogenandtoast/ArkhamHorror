module Arkham.Types.Card.PlayerCard.Cards.Evidence where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait hiding (Evidence)
import Arkham.Types.Window

newtype Evidence = Evidence Attrs
  deriving newtype (Show, ToJSON, FromJSON)

evidence :: CardId -> Evidence
evidence cardId = Evidence (event cardId "01022" "Evidence!" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = [Insight]
  , pcFast = True
  , pcWindows = setFromList [WhenEnemyDefeated You]
  }
