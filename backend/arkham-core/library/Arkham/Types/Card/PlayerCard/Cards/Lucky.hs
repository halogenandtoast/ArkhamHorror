module Arkham.Types.Card.PlayerCard.Cards.Lucky where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Lucky = Lucky Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env Lucky where
  runMessage msg (Lucky attrs) = Lucky <$> runMessage msg attrs

lucky :: CardId -> Lucky
lucky cardId = Lucky $ (event cardId "01080" "Lucky!" 1 Survivor)
  { pcTraits = [Fortune]
  , pcFast = True
  , pcWindows = setFromList [WhenWouldFailSkillTest You]
  }
