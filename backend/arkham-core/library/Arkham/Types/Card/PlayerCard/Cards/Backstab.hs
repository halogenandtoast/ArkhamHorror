module Arkham.Types.Card.PlayerCard.Cards.Backstab where

import ClassyPrelude

import qualified Arkham.Types.Action as Action
import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Backstab = Backstab Attrs
  deriving newtype (Show, ToJSON, FromJSON)

backstab :: CardId -> Backstab
backstab cardId = Backstab (event cardId "01051" "Backstab" 3 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Tactic]
  , pcAction = Just Action.Fight
  }
