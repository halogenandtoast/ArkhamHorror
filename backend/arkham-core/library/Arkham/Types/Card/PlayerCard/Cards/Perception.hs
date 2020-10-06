module Arkham.Types.Card.PlayerCard.Cards.Perception where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Perception = Perception Attrs
  deriving newtype (Show, ToJSON, FromJSON)

perception :: CardId -> Perception
perception cardId =
  Perception
    $ (skill
        cardId
        "01090"
        "Perception"
        [SkillIntellect, SkillIntellect]
        Neutral
      )
        { pcTraits = [Practiced]
        , pcCommitRestrictions = [MaxOnePerTest]
        }
