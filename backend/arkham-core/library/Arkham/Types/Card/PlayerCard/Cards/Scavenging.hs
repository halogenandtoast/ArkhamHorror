module Arkham.Types.Card.PlayerCard.Cards.Scavenging where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Scavenging = Scavenging Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Scavenging where
  runMessage msg (Scavenging attrs) = Scavenging <$> runMessage msg attrs

scavenging :: CardId -> Scavenging
scavenging cardId = Scavenging $ (asset cardId "01073" "Scavenging" 1 Survivor)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Talent]
  }

