module Arkham.Types.Card.PlayerCard.Cards.BulletproofVest3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype BulletproofVest3 = BulletproofVest3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env BulletproofVest3 where
  runMessage msg (BulletproofVest3 attrs) =
    BulletproofVest3 <$> runMessage msg attrs

bulletproofVest3 :: CardId -> BulletproofVest3
bulletproofVest3 cardId = BulletproofVest3
  (asset cardId "01094" "Bulletproof Vest" 2 Neutral)
    { pcSkills = [SkillCombat, SkillWild]
    , pcTraits = [Item, Armor]
    , pcLevel = 3
    }
