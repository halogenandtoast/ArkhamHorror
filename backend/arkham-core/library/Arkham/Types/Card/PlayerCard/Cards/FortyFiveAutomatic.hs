module Arkham.Types.Card.PlayerCard.Cards.FortyFiveAutomatic where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype FortyFiveAutomatic = FortyFiveAutomatic Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env FortyFiveAutomatic where
  runMessage msg (FortyFiveAutomatic attrs) =
    FortyFiveAutomatic <$> runMessage msg attrs

fortyFiveAutomatic :: CardId -> FortyFiveAutomatic
fortyFiveAutomatic cardId = FortyFiveAutomatic
  (asset cardId "01016" ".45 Automatic" 4 Guardian)
    { pcSkills = [SkillAgility]
    , pcTraits = [Item, Weapon, Firearm]
    }
