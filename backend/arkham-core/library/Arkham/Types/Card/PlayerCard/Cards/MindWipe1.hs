module Arkham.Types.Card.PlayerCard.Cards.MindWipe1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MindWipe1 = MindWipe1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindWipe1 :: CardId -> MindWipe1
mindWipe1 cardId = MindWipe1 $ (event cardId "01068" "Mind Wipe" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = [Spell]
  , pcLevel = 1
  , pcFast = True
  , pcWindows = setFromList [AnyPhaseBegins]
  }
