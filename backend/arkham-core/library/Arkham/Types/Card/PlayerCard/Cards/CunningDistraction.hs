module Arkham.Types.Card.PlayerCard.Cards.CunningDistraction where

import ClassyPrelude

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype CunningDistraction = CunningDistraction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env CunningDistraction where
  runMessage msg (CunningDistraction attrs) =
    CunningDistraction <$> runMessage msg attrs

cunningDistraction :: CardId -> CunningDistraction
cunningDistraction cardId = CunningDistraction
  (event cardId "01078" "Cunning Distraction" 5 Survivor)
    { pcSkills = [SkillIntellect, SkillWild]
    , pcTraits = [Tactic]
    , pcAction = Just Action.Evade
    }
