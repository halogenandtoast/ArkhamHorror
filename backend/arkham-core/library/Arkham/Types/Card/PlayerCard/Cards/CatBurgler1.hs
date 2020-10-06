module Arkham.Types.Card.PlayerCard.Cards.CatBurgler1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype CatBurgler1 = CatBurgler1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env CatBurgler1 where
  runMessage msg (CatBurgler1 attrs) = CatBurgler1 <$> runMessage msg attrs

catBurgler1 :: CardId -> CatBurgler1
catBurgler1 cardId = CatBurgler1 (asset cardId "01055" "Cat Burgler" 4 Rogue)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = [Ally, Criminal]
  }
