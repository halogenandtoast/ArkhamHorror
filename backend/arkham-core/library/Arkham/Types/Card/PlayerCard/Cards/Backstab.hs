module Arkham.Types.Card.PlayerCard.Cards.Backstab where

import ClassyPrelude

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Backstab = Backstab Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Backstab where
  runMessage msg (Backstab attrs) = Backstab <$> runMessage msg attrs

backstab :: CardId -> Backstab
backstab cardId = Backstab (event cardId "01051" "Backstab" 3 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Tactic]
  , pcAction = Just Action.Fight
  }
