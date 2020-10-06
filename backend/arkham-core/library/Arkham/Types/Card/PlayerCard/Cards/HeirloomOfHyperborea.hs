module Arkham.Types.Card.PlayerCard.Cards.HeirloomOfHyperborea where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg (HeirloomOfHyperborea attrs) =
    HeirloomOfHyperborea <$> runMessage msg attrs

heirloomOfHyperborea :: CardId -> HeirloomOfHyperborea
heirloomOfHyperborea cardId =
  HeirloomOfHyperborea
    $ (asset cardId "01012" "Heirloom of Hyperborea" 3 Neutral)
        { pcSkills = [SkillWillpower, SkillCombat, SkillWild]
        , pcTraits = [Item, Relic]
        }
