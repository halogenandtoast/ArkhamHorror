module Arkham.Types.Card.PlayerCard.Cards.OnTheLam where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype OnTheLam = OnTheLam Attrs
  deriving newtype (Show, ToJSON, FromJSON)

onTheLam :: CardId -> OnTheLam
onTheLam cardId = OnTheLam $ (event cardId "01010" "On the Lam" 1 Neutral)
  { pcTraits = [Tactic]
  , pcSkills = [SkillIntellect, SkillAgility, SkillWild, SkillWild]
  , pcFast = True
  , pcWindows = setFromList [AfterTurnBegins You, DuringTurn You]
  }
