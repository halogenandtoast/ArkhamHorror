module Arkham.Types.Card.PlayerCard.Cards.MedicalTexts where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype MedicalTexts = MedicalTexts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env MedicalTexts where
  runMessage msg (MedicalTexts attrs) = MedicalTexts <$> runMessage msg attrs

medicalTexts :: CardId -> MedicalTexts
medicalTexts cardId =
  MedicalTexts $ (asset cardId "01035" "Medical Texts" 2 Seeker)
    { pcSkills = [SkillCombat]
    , pcTraits = [Item, Tome]
    }
