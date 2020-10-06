module Arkham.Types.Card.PlayerCard.Cards.ExtraAmmunition1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ExtraAmmunition1 = ExtraAmmunition1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env ExtraAmmunition1 where
  runMessage msg (ExtraAmmunition1 attrs) =
    ExtraAmmunition1 <$> runMessage msg attrs

extraAmmunition1 :: CardId -> ExtraAmmunition1
extraAmmunition1 cardId = ExtraAmmunition1
  (event cardId "01026" "Extra Ammunition" 2 Guardian)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Supply]
    , pcLevel = 1
    }
