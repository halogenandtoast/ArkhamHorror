module Arkham.Types.Card.PlayerCard.Cards.ManualDexterity where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ManualDexterity = ManualDexterity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env ManualDexterity where
  runMessage msg (ManualDexterity attrs) =
    ManualDexterity <$> runMessage msg attrs

manualDexterity :: CardId -> ManualDexterity
manualDexterity cardId =
  ManualDexterity
    $ (skill
        cardId
        "01092"
        "Manual Dexterity"
        [SkillAgility, SkillAgility]
        Neutral
      )
        { pcTraits = [Innate]
        , pcCommitRestrictions = [MaxOnePerTest]
        }
