module Arkham.Types.Card.PlayerCard.Cards.JennysTwin45s where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype JennysTwin45s = JennysTwin45s Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env JennysTwin45s where
  runMessage msg (JennysTwin45s attrs) = JennysTwin45s <$> runMessage msg attrs

jennysTwin45s :: CardId -> JennysTwin45s
jennysTwin45s cardId =
  JennysTwin45s $ (asset cardId "02010" "Jenny's Twin .45s" 0 Neutral)
    { pcSkills = [SkillAgility, SkillAgility, SkillWild]
    , pcTraits = [Item, Weapon, Firearm]
    , pcCost = DynamicCost
    }
