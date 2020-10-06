module Arkham.Types.Card.PlayerCard.Cards.BeatCop2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype BeatCop2 = BeatCop2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env BeatCop2 where
  runMessage msg (BeatCop2 attrs) = BeatCop2 <$> runMessage msg attrs

beatCop2 :: CardId -> BeatCop2
beatCop2 cardId = BeatCop2 (asset cardId "01028" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Ally, Police]
  , pcLevel = 2
  }
