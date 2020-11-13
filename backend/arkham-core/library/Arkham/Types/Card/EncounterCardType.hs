module Arkham.Types.Card.EncounterCardType where

import ClassyPrelude
import Data.Aeson

data EncounterCardType
  = TreacheryType
  | EnemyType
  | LocationType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
