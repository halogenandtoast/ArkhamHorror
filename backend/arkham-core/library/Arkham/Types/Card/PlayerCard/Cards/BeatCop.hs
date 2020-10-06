module Arkham.Types.Card.PlayerCard.Cards.BeatCop where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype BeatCop = BeatCop Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env BeatCop where
  runMessage msg (BeatCop attrs) = BeatCop <$> runMessage msg attrs

beatCop :: CardId -> BeatCop
beatCop cardId = BeatCop (asset cardId "01018" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Ally, Police]
  }
