module Arkham.Types.LocationMatcher where

import ClassyPrelude

import Arkham.Types.LocationId
import Data.Aeson

newtype LocationMatcher = LocationNamed LocationName
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
