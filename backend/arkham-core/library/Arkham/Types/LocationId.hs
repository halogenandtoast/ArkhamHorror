module Arkham.Types.LocationId where

import Arkham.Types.Card
import ClassyPrelude
import Data.Aeson

newtype LocationId = LocationId { unLocationId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype ConnectedLocationId = ConnectedLocationId { unConnectedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype BlockedLocationId = BlockedLocationId { unBlockedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ClosestLocationId = ClosestLocationId { unClosestLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
