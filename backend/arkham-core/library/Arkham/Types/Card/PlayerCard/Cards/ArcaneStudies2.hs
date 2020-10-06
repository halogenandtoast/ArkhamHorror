module Arkham.Types.Card.PlayerCard.Cards.ArcaneStudies2 where

import ClassyPrelude

import Arkham.Types.Classes.RunMessage
import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ArcaneStudies2 = ArcaneStudies2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env ArcaneStudies2 where
  runMessage msg (ArcaneStudies2 attrs) =
    ArcaneStudies2 <$> runMessage msg attrs

arcaneStudies2 :: CardId -> ArcaneStudies2
arcaneStudies2 cardId = ArcaneStudies2
  (asset cardId "50007" "Arcane Studies" 0 Mystic)
    { pcSkills =
      [SkillWillpower, SkillWillpower, SkillIntellect, SkillIntellect]
    , pcTraits = [Talent]
    , pcLevel = 2
    }
