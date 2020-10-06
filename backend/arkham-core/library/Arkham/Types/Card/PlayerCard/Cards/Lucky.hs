module Arkham.Types.Card.PlayerCard.Cards.Lucky where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Lucky = Lucky Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky :: CardId -> Lucky
lucky cardId = Lucky $ (event cardId "01080" "Lucky!" 1 Survivor)
  { pcTraits = [Fortune]
  , pcFast = True
  , pcWindows = setFromList [WhenWouldFailSkillTest You]
  }
