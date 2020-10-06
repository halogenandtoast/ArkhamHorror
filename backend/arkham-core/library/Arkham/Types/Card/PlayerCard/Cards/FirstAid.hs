module Arkham.Types.Card.PlayerCard.Cards.FirstAid where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype FirstAid = FirstAid Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env FirstAid where
  runMessage msg (FirstAid attrs) = FirstAid <$> runMessage msg attrs

firstAid :: CardId -> FirstAid
firstAid cardId = FirstAid (asset cardId "01019" "First Aid" 2 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Talent, Science]
  }
