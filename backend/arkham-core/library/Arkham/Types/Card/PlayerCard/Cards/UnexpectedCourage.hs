module Arkham.Types.Card.PlayerCard.Cards.UnexpectedCourage where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype UnexpectedCourage = UnexpectedCourage Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env UnexpectedCourage where
  runMessage msg (UnexpectedCourage attrs) =
    UnexpectedCourage <$> runMessage msg attrs

unexpectedCourage :: CardId -> UnexpectedCourage
unexpectedCourage cardId =
  UnexpectedCourage
    $ (skill cardId "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
        { pcTraits = [Innate]
        , pcCommitRestrictions = [MaxOnePerTest]
        }
