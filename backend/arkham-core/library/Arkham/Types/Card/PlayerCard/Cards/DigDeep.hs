module Arkham.Types.Card.PlayerCard.Cards.DigDeep where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DigDeep = DigDeep Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env DigDeep where
  runMessage msg (DigDeep attrs) = DigDeep <$> runMessage msg attrs

digDeep :: CardId -> DigDeep
digDeep cardId = DigDeep (asset cardId "01077" "Dig Deep" 2 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Talent]
  }
