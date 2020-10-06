module Arkham.Types.Card.PlayerCard.Cards.DrawnToTheFlame where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DrawnToTheFlame = DrawnToTheFlame Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env DrawnToTheFlame where
  runMessage msg (DrawnToTheFlame attrs) =
    DrawnToTheFlame <$> runMessage msg attrs

drawnToTheFlame :: CardId -> DrawnToTheFlame
drawnToTheFlame cardId = DrawnToTheFlame
  (event cardId "01064" "Drawn to the Flame" 0 Mystic)
    { pcSkills = [SkillWillpower, SkillIntellect]
    , pcTraits = [Insight]
    }
