module Arkham.Types.Card.PlayerCard.Cards.SurvivalInstinct where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype SurvivalInstinct = SurvivalInstinct Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env SurvivalInstinct where
  runMessage msg (SurvivalInstinct attrs) =
    SurvivalInstinct <$> runMessage msg attrs

survivalInstinct :: CardId -> SurvivalInstinct
survivalInstinct cardId =
  SurvivalInstinct
    $ (skill cardId "01081" "Survival Instrinct" [SkillAgility] Survivor)
        { pcTraits = [Innate]
        }
