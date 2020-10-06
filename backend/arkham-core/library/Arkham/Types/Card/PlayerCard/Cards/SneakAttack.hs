module Arkham.Types.Card.PlayerCard.Cards.SneakAttack where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype SneakAttack = SneakAttack Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env SneakAttack where
  runMessage msg (SneakAttack attrs) = SneakAttack <$> runMessage msg attrs

sneakAttack :: CardId -> SneakAttack
sneakAttack cardId = SneakAttack $ (event cardId "01052" "Sneak Attack" 2 Rogue
                                   )
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = [Tactic]
  }
