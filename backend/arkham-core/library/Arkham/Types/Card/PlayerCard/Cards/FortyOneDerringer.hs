module Arkham.Types.Card.PlayerCard.Cards.FortyOneDerringer where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype FortyOneDerringer = FortyOneDerringer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env FortyOneDerringer where
  runMessage msg (FortyOneDerringer attrs) =
    FortyOneDerringer <$> runMessage msg attrs

fortyOneDerringer :: CardId -> FortyOneDerringer
fortyOneDerringer cardId = FortyOneDerringer
  (asset cardId "01047" ".41 Derringer" 3 Rogue)
    { pcSkills = [SkillCombat]
    , pcTraits = [Item, Weapon, Firearm, Illicit]
    }
