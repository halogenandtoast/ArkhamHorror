module Arkham.Types.Card.PlayerCard.Cards.LeatherCoat where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype LeatherCoat = LeatherCoat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env LeatherCoat where
  runMessage msg (LeatherCoat attrs) = LeatherCoat <$> runMessage msg attrs

leatherCoat :: CardId -> LeatherCoat
leatherCoat cardId =
  LeatherCoat $ (asset cardId "01072" "Leather Coat" 0 Survivor)
    { pcSkills = [SkillCombat]
    , pcTraits = [Item, Armor]
    }
