module Arkham.Types.Asset.Uses where

import ClassyPrelude
import Data.Aeson

data UseType = Ammo | Supply | Secret | Charge | Try | Bounty | Whistle | Resource | Key
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Uses = NoUses | Uses UseType Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
