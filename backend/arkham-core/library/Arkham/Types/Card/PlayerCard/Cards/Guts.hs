module Arkham.Types.Card.PlayerCard.Cards.Guts where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Guts = Guts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Guts where
  runMessage msg (Guts attrs) = Guts <$> runMessage msg attrs

guts :: CardId -> Guts
guts cardId = Guts
  (skill cardId "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { pcTraits = [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }
