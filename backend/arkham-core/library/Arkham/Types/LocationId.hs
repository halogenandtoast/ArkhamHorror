module Arkham.Types.LocationId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id

newtype LocationId = LocationId { unLocationId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype ConnectedLocationId = ConnectedLocationId { unConnectedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype BlockedLocationId = BlockedLocationId { unBlockedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype AccessibleLocationId = AccessibleLocationId { unAccessibleLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype EnemyAccessibleLocationId = EnemyAccessibleLocationId { unEnemyAccessibleLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ClosestLocationId = ClosestLocationId { unClosestLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

-- | this is specifically to handle the next step in a path
newtype ClosestPathLocationId = ClosestPathLocationId { unClosestPathLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype FarthestLocationId = FarthestLocationId { unFarthestLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype EmptyLocationId = EmptyLocationId { unEmptyLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype RevealedLocationId = RevealedLocationId { unRevealedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype UnrevealedLocationId = UnrevealedLocationId { unUnrevealedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype SetAsideLocationCardCode = SetAsideLocationCardCode { unSetAsideLocationCardCode :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
