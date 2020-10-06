module Arkham.Types.Card.PlayerCard.Cards.Overpower where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Overpower = Overpower Attrs
  deriving newtype (Show, ToJSON, FromJSON)

overpower :: CardId -> Overpower
overpower cardId =
  Overpower
    $ (skill cardId "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
        { pcTraits = [Practiced]
        , pcCommitRestrictions = [MaxOnePerTest]
        }
