module Arkham.Types.Card.PlayerCard.Cards.Deduction where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Deduction = Deduction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Deduction where
  runMessage msg (Deduction attrs) = Deduction <$> runMessage msg attrs

deduction :: CardId -> Deduction
deduction cardId = Deduction
  (skill cardId "01039" "Deduction" [SkillIntellect] Seeker)
    { pcTraits = [Practiced]
    }
