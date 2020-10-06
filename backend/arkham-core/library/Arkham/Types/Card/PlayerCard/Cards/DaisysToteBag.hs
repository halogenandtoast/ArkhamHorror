module Arkham.Types.Card.PlayerCard.Cards.DaisysToteBag where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DaisysToteBag = DaisysToteBag Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env DaisysToteBag where
  runMessage msg (DaisysToteBag attrs) = DaisysToteBag <$> runMessage msg attrs

daisysToteBag :: CardId -> DaisysToteBag
daisysToteBag cardId = DaisysToteBag
  (asset cardId "01008" "Daisy's Tote Bag" 2 Neutral)
    { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
    , pcTraits = [Item]
    }
