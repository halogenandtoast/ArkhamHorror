module Arkham.Types.Card.PlayerCard.Cards.Opportunist where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Opportunist = Opportunist Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Opportunist where
  runMessage msg (Opportunist attrs) = Opportunist <$> runMessage msg attrs

opportunist :: CardId -> Opportunist
opportunist cardId =
  Opportunist $ (skill cardId "01053" "Opportunist" [SkillWild] Rogue)
    { pcTraits = [Innate]
    , pcCommitRestrictions = [OnlyYourTest]
    }
