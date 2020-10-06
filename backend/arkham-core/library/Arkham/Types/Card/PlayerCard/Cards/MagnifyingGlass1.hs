module Arkham.Types.Card.PlayerCard.Cards.MagnifyingGlass1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MagnifyingGlass1 = MagnifyingGlass1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env MagnifyingGlass1 where
  runMessage msg (MagnifyingGlass1 attrs) =
    MagnifyingGlass1 <$> runMessage msg attrs

magnifyingGlass1 :: CardId -> MagnifyingGlass1
magnifyingGlass1 cardId =
  MagnifyingGlass1 $ (asset cardId "01040" "Magnifying Glass" 0 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Item, Tool]
    , pcFast = True
    , pcWindows = setFromList [DuringTurn You]
    , pcLevel = 1
    }
