module Arkham.Types.Card.PlayerCard.Cards.StrayCat where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype StrayCat = StrayCat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env StrayCat where
  runMessage msg (StrayCat attrs) = StrayCat <$> runMessage msg attrs

strayCat :: CardId -> StrayCat
strayCat cardId = StrayCat $ (asset cardId "01076" "Stray Cat" 1 Survivor)
  { pcSkills = [SkillAgility]
  , pcTraits = [Ally, Creature]
  }
