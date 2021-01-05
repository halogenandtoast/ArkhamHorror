module Arkham.Types.LocationId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Helpers
import qualified Data.Char as Char

data EmptyLocation = EmptyLocation

data LocationName = LocationName
  { locationNameTitle :: Text
  , locationNameSubtitle :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

locationNameToLabel :: LocationName -> Text
locationNameToLabel (LocationName title _) =
  pack . toLabel . replaceNonLetters . unpack $ title
 where
  capitalize (x : xs) = Char.toUpper x : xs
  capitalize [] = ""

newtype LocationId = LocationId { unLocationId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype ConnectedLocationId = ConnectedLocationId { unConnectedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype BlockedLocationId = BlockedLocationId { unBlockedLocationId :: LocationId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype AccessibleLocationId = AccessibleLocationId { unAccessibleLocationId :: LocationId }
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
