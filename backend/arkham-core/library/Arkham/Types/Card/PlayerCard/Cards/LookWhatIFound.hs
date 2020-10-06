module Arkham.Types.Card.PlayerCard.Cards.LookWhatIFound where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype LookWhatIFound = LookWhatIFound Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env LookWhatIFound where
  runMessage msg (LookWhatIFound attrs) =
    LookWhatIFound <$> runMessage msg attrs

lookWhatIFound :: CardId -> LookWhatIFound
lookWhatIFound cardId =
  LookWhatIFound $ (event cardId "01079" "\"Look what I found!\"" 2 Survivor)
    { pcSkills = [SkillIntellect, SkillIntellect]
    , pcTraits = [Fortune]
    , pcFast = True
    , pcWindows = setFromList [ AfterFailSkillTest You n | n <- [0 .. 2] ]
    }
