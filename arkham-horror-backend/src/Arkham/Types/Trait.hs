module Arkham.Types.Trait where

import ClassyPrelude
import Data.Aeson

data ArkhamTrait = Tome | Ghoul | Creature | Monster | Humanoid
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

