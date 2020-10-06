module Arkham.Types.Card.PlayerCard.Cards.ArcaneInitiate where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ArcaneInitiate = ArcaneInitiate Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneInitiate :: CardId -> ArcaneInitiate
arcaneInitiate cardId = ArcaneInitiate
  (asset cardId "01063" "Arcane Initiate" 1 Mystic)
    { pcSkills = [SkillWillpower]
    , pcTraits = [Ally, Sorcerer]
    }
