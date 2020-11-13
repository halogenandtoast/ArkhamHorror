module Arkham.Types.Card.EncounterCardMatcher where

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCardType
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

data EncounterCardMatcher = EncounterCardMatchByType (EncounterCardType, Maybe Trait) | EncounterCardMatchByCardCode CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

