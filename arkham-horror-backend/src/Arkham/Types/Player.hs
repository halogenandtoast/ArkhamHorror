module Arkham.Types.Player where

import Arkham.Types.Card
import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson

data ArkhamPlayer = ArkhamPlayer
  { investigator :: ArkhamInvestigator
  , sanityDamage :: Int
  , healthDamage :: Int
  , resources :: Int
  , clues :: Int
  , hand :: [ArkhamCard]
  , inPlay :: [ArkhamCard]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
