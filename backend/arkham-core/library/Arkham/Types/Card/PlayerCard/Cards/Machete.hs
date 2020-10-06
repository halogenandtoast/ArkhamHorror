module Arkham.Types.Card.PlayerCard.Cards.Machete where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Machete = Machete Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Machete where
  runMessage msg (Machete attrs) = Machete <$> runMessage msg attrs

machete :: CardId -> Machete
machete cardId = Machete $ (asset cardId "01020" "Machete" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }
