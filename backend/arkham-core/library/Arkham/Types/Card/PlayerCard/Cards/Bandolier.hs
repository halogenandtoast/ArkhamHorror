module Arkham.Types.Card.PlayerCard.Cards.Bandolier where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Bandolier = Bandolier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Bandolier where
  runMessage msg (Bandolier attrs) = Bandolier <$> runMessage msg attrs

bandolier :: CardId -> Bandolier
bandolier cardId = Bandolier (asset cardId "02147" "Bandolier" 2 Guardian)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = [Item]
  }
