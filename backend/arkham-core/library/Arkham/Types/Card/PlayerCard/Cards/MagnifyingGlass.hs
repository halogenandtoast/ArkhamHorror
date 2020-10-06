module Arkham.Types.Card.PlayerCard.Cards.MagnifyingGlass where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MagnifyingGlass = MagnifyingGlass Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass :: CardId -> MagnifyingGlass
magnifyingGlass cardId =
  MagnifyingGlass $ (asset cardId "01030" "Magnifying Glass" 1 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Item, Tool]
    , pcFast = True
    , pcWindows = setFromList [DuringTurn You]
    }
