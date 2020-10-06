module Arkham.Types.Card.PlayerCard.Cards.MindWipe3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MindWipe3 = MindWipe3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindWipe3 :: CardId -> MindWipe3
mindWipe3 cardId = MindWipe3 $ (event cardId "50008" "Mind Wipe" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = [Spell]
  , pcLevel = 3
  , pcFast = True
  , pcWindows = setFromList [AnyPhaseBegins]
  }
