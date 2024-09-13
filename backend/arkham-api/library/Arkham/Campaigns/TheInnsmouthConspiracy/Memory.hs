module Arkham.Campaigns.TheInnsmouthConspiracy.Memory where

import Arkham.Prelude

data Memory = AMeetingWithThomasDawson
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
