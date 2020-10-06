module Arkham.Types.Card.PlayerCard.Cards.RabbitsFoot where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype RabbitsFoot = RabbitsFoot Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env RabbitsFoot where
  runMessage msg (RabbitsFoot attrs) = RabbitsFoot <$> runMessage msg attrs

rabbitsFoot :: CardId -> RabbitsFoot
rabbitsFoot cardId =
  RabbitsFoot $ (asset cardId "01075" "Rabbit's Foot" 1 Survivor)
    { pcSkills = [SkillWild]
    , pcTraits = [Item, Charm]
    }
