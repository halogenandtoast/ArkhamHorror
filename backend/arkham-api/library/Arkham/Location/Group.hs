{- | Location groups: several locations drawn as one box on the map.

A scenario whose locations come in interchangeable clusters — Red Sunrise's rows of
same-named Woods, Lost in Time and Space's repeated Prism/Tear locations — draws an
unreadable tangle on a flat grid, because every member of a cluster connects to every
member of the next. Declaring the cluster a /group/ lets the frontend draw one box and
route connections to and from the box instead of to each location inside it.

The scenario declares its groups; each location then records which group it joined and
at which index, so the order inside the box is stable across reloads, undo and replay
rather than being whatever order a query happened to return.
-}
module Arkham.Location.Group where

import Arkham.Prelude

-- | How a group arranges its members inside its box.
data GroupLayout
  = -- | One row, left to right.
    GroupRow
  | -- | One column, top to bottom.
    GroupColumn
  | {- | The squarest arrangement that fits: 1 member is 1x1, 2-4 are 2x2, 5-9 are
    3x3. Members fill left to right, top to bottom.
    -}
    GroupSquare
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LocationGroupKey = LocationGroupKey {unLocationGroupKey :: Text}
  deriving stock Data
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | A group as the scenario declares it. Membership lives on the locations.
data LocationGroup = LocationGroup
  { groupKey :: LocationGroupKey
  , groupLayout :: GroupLayout
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

instance ToJSON LocationGroup where
  toJSON (LocationGroup k l) = object ["key" .= k, "layout" .= l]

instance FromJSON LocationGroup where
  parseJSON = withObject "LocationGroup" \o -> LocationGroup <$> o .: "key" <*> o .: "layout"

-- | A location's place in a group: which group, and its stable index within it.
data GroupMembership = GroupMembership
  { membershipKey :: LocationGroupKey
  , membershipIndex :: Int
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

instance ToJSON GroupMembership where
  toJSON (GroupMembership k i) = object ["key" .= k, "index" .= i]

instance FromJSON GroupMembership where
  parseJSON = withObject "GroupMembership" \o -> GroupMembership <$> o .: "key" <*> o .: "index"

-- | Side length of the square a group of @n@ members is drawn in.
groupSquareSide :: Int -> Int
groupSquareSide n = ceiling @Double . sqrt . fromIntegral $ max 1 n

{- | Row and column of the @i@th member, for a group of @n@ members. The frontend
draws from this, and it is here so the backend and frontend cannot disagree about it.
-}
groupSlot :: GroupLayout -> Int -> Int -> (Int, Int)
groupSlot layout n i = case layout of
  GroupRow -> (0, i)
  GroupColumn -> (i, 0)
  GroupSquare -> let side = groupSquareSide n in (i `div` side, i `mod` side)
