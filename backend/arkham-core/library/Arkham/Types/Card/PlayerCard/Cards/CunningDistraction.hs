module Arkham.Types.Card.PlayerCard.Cards.CunningDistraction where

import ClassyPrelude

import qualified Arkham.Types.Action as Action
import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype CunningDistraction = CunningDistraction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cunningDistraction :: CardId -> CunningDistraction
cunningDistraction cardId = CunningDistraction
  (event cardId "01078" "Cunning Distraction" 5 Survivor)
    { pcSkills = [SkillIntellect, SkillWild]
    , pcTraits = [Tactic]
    , pcAction = Just Action.Evade
    }
