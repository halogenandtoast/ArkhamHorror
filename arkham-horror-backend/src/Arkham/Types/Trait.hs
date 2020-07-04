module Arkham.Types.Trait where

import ClassyPrelude
import Data.Aeson

data ArkhamTrait = Tome | Ghoul
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

