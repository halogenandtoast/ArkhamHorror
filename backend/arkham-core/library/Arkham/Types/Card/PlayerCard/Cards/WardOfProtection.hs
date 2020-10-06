module Arkham.Types.Card.PlayerCard.Cards.WardOfProtection where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype WardOfProtection = WardOfProtection Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env WardOfProtection where
  runMessage msg (WardOfProtection attrs) =
    WardOfProtection <$> runMessage msg attrs

wardOfProtection :: CardId -> WardOfProtection
wardOfProtection cardId =
  WardOfProtection $ (event cardId "01065" "Ward of Protection" 1 Mystic)
    { pcSkills = [SkillWild]
    , pcTraits = [Spell, Spirit]
    , pcFast = True
    , pcWindows = setFromList [WhenDrawTreachery You False]
    }
