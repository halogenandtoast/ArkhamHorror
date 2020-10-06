module Arkham.Types.Card.PlayerCard.Cards.Elusive where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Elusive = Elusive Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Elusive where
  runMessage msg (Elusive attrs) = Elusive <$> runMessage msg attrs

elusive :: CardId -> Elusive
elusive cardId = Elusive (event cardId "01050" "Elusive" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }
