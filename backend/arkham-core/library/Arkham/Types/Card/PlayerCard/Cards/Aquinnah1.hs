module Arkham.Types.Card.PlayerCard.Cards.Aquinnah1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Aquinnah1 = Aquinnah1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Aquinnah1 where
  runMessage msg (Aquinnah1 attrs) = Aquinnah1 <$> runMessage msg attrs

aquinnah1 :: CardId -> Aquinnah1
aquinnah1 cardId = Aquinnah1 (asset cardId "01082" "Aquinnah" 5 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Ally]
  }
