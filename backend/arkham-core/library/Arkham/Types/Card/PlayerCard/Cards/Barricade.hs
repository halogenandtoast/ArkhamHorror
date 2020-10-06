module Arkham.Types.Card.PlayerCard.Cards.Barricade where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Barricade = Barricade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Barricade where
  runMessage msg (Barricade attrs) = Barricade <$> runMessage msg attrs

barricade :: CardId -> Barricade
barricade cardId = Barricade (event cardId "01038" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = [Insight, Tactic]
  }
