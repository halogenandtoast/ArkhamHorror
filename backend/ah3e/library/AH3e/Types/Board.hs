module AH3e.Types.Board where

import AH3e.Prelude
import AH3e.Types.Ids
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

data StreetType = Residential | Bridge | Scenic
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data RouteType = CountryRoad | FerryTerminal | TrainPlatform
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data ThresholdType = HiddenPath | DerelictPortal | WildGateway
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data SpaceKind
  = LocationSpace
  | StreetSpace StreetType
  | TravelRouteSpace RouteType
  | MysterySpace
  | ThresholdSpace ThresholdType
  | SpecialSpace
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Hazard = HazardDamage | HazardHorror | HazardFocus
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Town = Arkham | Innsmouth | Kingsport | OtherWorld
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Marker = Marker {color :: Text, faceUp :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Space = Space
  { id :: SpaceId
  , name :: Text
  , kind :: SpaceKind
  , neighborhood :: Maybe NeighborhoodId
  , doom :: Int
  , clues :: Int
  , markers :: [Marker]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Neighborhood = Neighborhood
  { id :: NeighborhoodId
  , name :: Text
  , town :: Town
  , spaces :: [SpaceId]
  , clues :: Int
  , anomaly :: Bool
  , terror :: Int
  , attachedTerror :: [CardId]
  , markers :: [Marker]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TilePlacement = TilePlacement {neighborhood :: NeighborhoodId, x :: Double, y :: Double}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data StreetPlacement = StreetPlacement {space :: SpaceId, x :: Double, y :: Double, angle :: Double}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpaceAnchor = SpaceAnchor {space :: SpaceId, x :: Double, y :: Double}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- positions in units of a neighborhood tile's flat-to-flat width, y pointing down
data BoardLayout = BoardLayout
  { tiles :: [TilePlacement]
  , streets :: [StreetPlacement]
  , anchors :: [SpaceAnchor]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyLayout :: BoardLayout
emptyLayout = BoardLayout [] [] []

data Board = Board
  { spaces :: Map SpaceId Space
  , neighborhoods :: Map NeighborhoodId Neighborhood
  , borders :: Map SpaceId (Map SpaceId (Maybe Hazard))
  , layout :: BoardLayout
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyBoard :: Board
emptyBoard = Board mempty mempty mempty emptyLayout

isStreetLike :: SpaceKind -> Bool
isStreetLike = \case
  StreetSpace _ -> True
  TravelRouteSpace _ -> True
  ThresholdSpace _ -> True
  _ -> False

isNeighborhoodSpace :: SpaceKind -> Bool
isNeighborhoodSpace = \case
  LocationSpace -> True
  MysterySpace -> True
  _ -> False

adjacentSpaces :: SpaceId -> Board -> [SpaceId]
adjacentSpaces sid board = maybe [] Map.keys (Map.lookup sid board.borders)

borderHazard :: SpaceId -> SpaceId -> Board -> Maybe Hazard
borderHazard a b board = join (Map.lookup a board.borders >>= Map.lookup b)

routeType :: Space -> Maybe RouteType
routeType s = case s.kind of
  TravelRouteSpace r -> Just r
  _ -> Nothing

sameRouteSpaces :: SpaceId -> Board -> [SpaceId]
sameRouteSpaces sid board = case Map.lookup sid board.spaces >>= routeType of
  Nothing -> []
  Just r -> [s.id | s <- Map.elems board.spaces, s.id /= sid, routeType s == Just r]

-- monsters treat same-type travel routes as adjacent
monsterAdjacent :: SpaceId -> Board -> [SpaceId]
monsterAdjacent sid board = Set.toList $ Set.fromList (adjacentSpaces sid board <> sameRouteSpaces sid board)

distancesFrom :: (SpaceId -> [SpaceId]) -> SpaceId -> Map SpaceId Int
distancesFrom nbrs start = go (Map.singleton start 0) [start]
 where
  go seen [] = seen
  go seen (x : xs) =
    let d = seen Map.! x
        new = [n | n <- nbrs x, not (Map.member n seen)]
        seen' = foldr (\n -> Map.insert n (d + 1)) seen new
     in go seen' (xs <> new)

neighborhoodSpaces :: NeighborhoodId -> Board -> [SpaceId]
neighborhoodSpaces nid board = [s.id | s <- Map.elems board.spaces, s.neighborhood == Just nid]

neighborhoodDoom :: NeighborhoodId -> Board -> Int
neighborhoodDoom nid board = sum [s.doom | s <- Map.elems board.spaces, s.neighborhood == Just nid]

spaceNeighborhood :: SpaceId -> Board -> Maybe NeighborhoodId
spaceNeighborhood sid board = Map.lookup sid board.spaces >>= (.neighborhood)
