module Arkham.Types.Card.PlayerCard.Cards.RabbitsFoot3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype RabbitsFoot3 = RabbitsFoot3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env RabbitsFoot3 where
  runMessage msg (RabbitsFoot3 attrs) = RabbitsFoot3 <$> runMessage msg attrs

rabbitsFoot3 :: CardId -> RabbitsFoot3
rabbitsFoot3 cardId =
  RabbitsFoot3 $ (asset cardId "50010" "Rabbit's Foot" 1 Survivor)
    { pcSkills = [SkillWild]
    , pcTraits = [Item, Charm]
    , pcLevel = 3
    }
