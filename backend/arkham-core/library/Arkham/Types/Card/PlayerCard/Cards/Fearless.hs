module Arkham.Types.Card.PlayerCard.Cards.Fearless where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Fearless = Fearless Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fearless :: CardId -> Fearless
fearless cardId = Fearless
  (skill cardId "01067" "Fearless" [SkillWillpower] Mystic)
    { pcTraits = [Innate]
    }
