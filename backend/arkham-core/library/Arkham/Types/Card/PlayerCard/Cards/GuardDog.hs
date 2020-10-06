module Arkham.Types.Card.PlayerCard.Cards.GuardDog where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype GuardDog = GuardDog Attrs
  deriving newtype (Show, ToJSON, FromJSON)

guardDog :: CardId -> GuardDog
guardDog cardId = GuardDog (asset cardId "01021" "Guard Dog" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Ally, Creature]
  }
