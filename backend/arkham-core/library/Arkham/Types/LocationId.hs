module Arkham.Types.LocationId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype LocationLabel = LocationLabel { unLocationLabel :: Text }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype LocationId = LocationId { unLocationId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)

newtype ConnectedLocationId = ConnectedLocationId { unConnectedLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype BlockedLocationId = BlockedLocationId { unBlockedLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AccessibleLocationId = AccessibleLocationId { unAccessibleLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EnemyAccessibleLocationId = EnemyAccessibleLocationId { unEnemyAccessibleLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ClosestLocationId = ClosestLocationId { unClosestLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | this is specifically to handle the next step in a path
newtype ClosestPathLocationId = ClosestPathLocationId { unClosestPathLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype FarthestLocationId = FarthestLocationId { unFarthestLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EmptyLocationId = EmptyLocationId { unEmptyLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype RevealedLocationId = RevealedLocationId { unRevealedLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype UnrevealedLocationId = UnrevealedLocationId { unUnrevealedLocationId :: LocationId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
