module AH3e.Content.Tiles (
  Variety (..),
  Edge (..),
  TileDef (..),
  hazardous,
  StreetDef (..),
  RouteDef (..),
  MysteryTile (..),
  ThresholdTile (..),
  CornerTile (..),
  ClusterLink (..),
  nudge,
  tileStep,
  Pieces (..),
  noPieces,
  tiles,
  tile,
  spaceIdFor,
  edgeSpaces,
  aroundFrom,
  cornerSeat,
  CornerSeat (..),
  ringAround,
  buildMap,
  buildMapWith,
  buildMapLaidOut,
  buildMapOf,
  addedMap,
  addedMapOf,
) where

import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Ids
import Data.Char (isAlphaNum, toLower)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text qualified as T

data Variety = V1 | V2
  deriving stock (Show, Eq)

data Edge = TopLeft | TopRight | SideLeft | SideRight | BottomLeft | BottomRight
  deriving stock (Show, Eq)

data TileDef = TileDef
  { neighborhood :: NeighborhoodId
  , name :: Text
  , town :: Town
  , variety :: Variety
  , spaces :: (Text, Text, Text)
  , hazards :: [(Text, Text, Hazard)]
  {- ^ borders between two of this tile's own spaces that cost something to cross.
  Other worlds have them printed on the tile (Secrets of the Order, p. 4).
  -}
  }

data StreetDef = StreetDef
  { from :: NeighborhoodId
  , edge :: Edge
  , to :: NeighborhoodId
  , streetType :: StreetType
  }

{- | A travel route hanging off one tile's edge. It is a space of its own,
bordering only that tile; the engine already treats routes of the same type as
adjacent to each other, so nothing joins them here.
-}
data RouteDef = RouteDef {from :: NeighborhoodId, edge :: Edge, routeType :: RouteType}

{- | A mystery attached to one tile's edge (Devil Reef, the Strange High House).
Unlike a street or a route it belongs to the neighborhood it hangs off, so doom
placed there counts toward that neighborhood's total (Under Dark Waves, p. 8).
-}
data MysteryTile = MysteryTile {from :: NeighborhoodId, edge :: Edge, name :: Text}

{- | A threshold tile laid between two hexes, the way a street is: its space borders
the spaces along both edges it touches, and the tiles sit as far apart as a street
would hold them. Derelict portals are these (Secrets of the Order, p. 4).
-}
data ThresholdTile = ThresholdTile
  { from :: NeighborhoodId
  , edge :: Edge
  , to :: NeighborhoodId
  , thresholdType :: ThresholdType
  , hazards :: [Hazard]
  -- ^ the icons printed along its borders; setup decides which lands where
  }

{- | A threshold tile laid in the corner where hexes meet, which is what a hidden path
is. It borders the one space of each tile that owns that corner and nothing else --
not even a street running between two of them -- so the tiles are named and the rest
is read off the geometry: the corner is where their centres average out, and the space
each tile puts there is the one whose wedge faces it.
-}
data CornerTile = CornerTile
  { tiles :: [NeighborhoodId]
  -- ^ the tile it is wedged against first, then the others it reaches
  , thresholdType :: ThresholdType
  , hazards :: [Hazard]
  -- ^ the icons printed along its borders; setup decides which lands where
  }

{- | Where a cluster of tiles no street reaches is set out: the edge of an already
placed tile it is laid against. Nothing connects along it -- it only says where
the tiles go, the way they would be put down on the table.
-}
data ClusterLink = ClusterLink {from :: NeighborhoodId, edge :: Edge, to :: NeighborhoodId}

{- | Everything a map has besides its hexes and the streets between them. A sheet
names only the pieces it uses, so this is built from 'noPieces'.
-}
data Pieces = Pieces
  { routes :: [RouteDef]
  , mysteries :: [MysteryTile]
  , thresholds :: [ThresholdTile]
  , corners :: [CornerTile]
  , clusters :: [ClusterLink]
  , nudges :: [(NeighborhoodId, (Double, Double))]
  -- ^ tiles shifted off the lattice by hand, in tile widths; see 'nudge'
  }

noPieces :: Pieces
noPieces = Pieces {routes = [], mysteries = [], thresholds = [], corners = [], clusters = [], nudges = []}

{- | A tile shifted off the lattice, so much right and so much up, in tile widths. A sheet
sometimes sets a loose tile down a little clear of the rest rather than square against it;
everything hanging off that tile -- its travel routes, its mysteries, any street to it --
is placed from its middle, so it all goes along.
-}
nudge :: NeighborhoodId -> Double -> Double -> (NeighborhoodId, (Double, Double))
nudge nid right up = (nid, (right, -up))

slug :: Text -> Text
slug =
  T.intercalate "-"
    . T.words
    . T.map (\c -> if isAlphaNum c then toLower c else ' ')
    . T.filter (/= '\'')

spaceIdFor :: Text -> SpaceId
spaceIdFor = SpaceId . slug

mkTile :: Text -> Town -> Variety -> (Text, Text, Text) -> TileDef
mkTile n t v sps = TileDef (NeighborhoodId (slug n)) n t v sps []

-- | The hazardous borders printed between a tile's own spaces, named either way round.
hazardous :: [(Text, Text, Hazard)] -> TileDef -> TileDef
hazardous hs t = TileDef t.neighborhood t.name t.town t.variety t.spaces hs

tiles :: [TileDef]
tiles =
  [ mkTile "Easttown" Arkham V1 ("Velma's Diner", "Hibb's Roadhouse", "Police Station")
  , mkTile "Downtown" Arkham V2 ("Independence Square", "La Bella Luna", "Arkham Asylum")
  , mkTile "Northside" Arkham V2 ("Arkham Advertiser", "Train Station", "Curiositie Shoppe")
  , mkTile "Rivertown" Arkham V1 ("Black Cave", "Graveyard", "General Store")
  , mkTile "Merchant District" Arkham V2 ("Unvisited Isle", "Tick-Tock Club", "River Docks")
  , mkTile "Miskatonic University" Arkham V2 ("Observatory", "Science Building", "Orne Library")
  , mkTile "Uptown" Arkham V1 ("Hangman's Hill", "St. Mary's Hospital", "Ye Olde Magick Shoppe")
  , mkTile "Southside" Arkham V2 ("Ma's Boarding House", "Historical Society", "South Church")
  , mkTile "French Hill" Arkham V2 ("Bayfriar Gardens", "Duterte Funeral Home", "Silver Twilight Lodge")
  , -- an other world rather than a part of Arkham, and its borders are hazardous
    hazardous
      [ ("City of the Gugs", "Vale of Pnath", HazardHorror)
      , ("City of the Gugs", "Vaults of Zin", HazardDamage)
      , ("Vaults of Zin", "Vale of Pnath", HazardFocus)
      ]
      (mkTile "The Underworld" OtherWorld V1 ("City of the Gugs", "Vaults of Zin", "Vale of Pnath"))
  , mkTile
      "Innsmouth Village"
      Innsmouth
      V2
      ("Esoteric Order of Dagon", "First National Grocery", "Innsmouth Jail")
  , mkTile "Innsmouth Shore" Innsmouth V1 ("Marsh Refinery", "Falcon Point", "Gilman House")
  , mkTile
      "Central Kingsport"
      Kingsport
      V1
      ("Congregational Hospital", "Neil's Curiosity Shop", "Hall School")
  , mkTile
      "Kingsport Harbor"
      Kingsport
      V2
      ("North Point Lighthouse", "The Rope and Anchor", "St. Erasmus's Home")
  ]

tile :: NeighborhoodId -> TileDef
tile nid = fromJustNote ("unknown tile " <> show nid) $ listToMaybe [t | t <- tiles, t.neighborhood == nid]

data Slot = A | B | C
  deriving stock Eq

-- each space covers two of the six vertex wedges; an edge borders the spaces
-- owning the wedges at its two ends
edgeSlots :: Variety -> Edge -> [Slot]
edgeSlots V1 = \case
  TopLeft -> [A]
  TopRight -> [A, B]
  SideRight -> [B]
  BottomRight -> [B, C]
  BottomLeft -> [C]
  SideLeft -> [C, A]
edgeSlots V2 = \case
  TopLeft -> [C, A]
  TopRight -> [A]
  SideRight -> [A, B]
  BottomRight -> [B]
  BottomLeft -> [B, C]
  SideLeft -> [C]

opposite :: Edge -> Edge
opposite = \case
  TopLeft -> BottomRight
  TopRight -> BottomLeft
  SideLeft -> SideRight
  SideRight -> SideLeft
  BottomLeft -> TopRight
  BottomRight -> TopLeft

-- screen angle (y down) of each edge's outward normal on a regular pointy-top hex
edgeAngle :: Edge -> Double
edgeAngle e = deg * pi / 180
 where
  deg = case e of
    SideRight -> 0
    BottomRight -> 60
    BottomLeft -> 120
    SideLeft -> 180
    TopLeft -> 240
    TopRight -> 300

-- distance from the center to every edge, in units of the tile's flat-to-flat width
edgeApothem :: Edge -> Double
edgeApothem _ = 0.5

-- the middle of the wedge each space covers
slotAngle :: Variety -> Slot -> Double
slotAngle v s = deg * pi / 180
 where
  deg = case (v, s) of
    (V1, A) -> 240
    (V1, B) -> 0
    (V1, C) -> 120
    (V2, A) -> 300
    (V2, B) -> 60
    (V2, C) -> 180

{- | The gap between two tiles, in tile widths, which is what a street spans. Measured
off the tabletop version: its tiles sit 1.279 of a tile picture apart, and a picture is
wider than the hexagon in it by 1/0.9548, so in hexagons that is 1.339 centre to centre.
-}
-- | One step of the lattice: how far apart two neighbouring tiles' middles sit.
tileStep :: Double
tileStep = 2 * edgeApothem SideLeft + streetLength

streetLength, anchorRadius :: Double
streetLength = 0.3395
anchorRadius = 0.3

{- | How deep a connector is drawn and how much of that depth is the tab it joins
by, both as a fraction of the tile's width (the frontend's @CONNECTOR_W@ over the
art's own proportions comes to the first). A connector is not street sized, so it
is seated by its own depth rather than in the slot a street would take: its
shoulder lands on the tile's edge and the tab reaches into the notch.
-}
connectorDepth, connectorTab :: Double
connectorDepth = 0.349
connectorTab = 0.2

{- | The daylight left between a hanging piece and the tile it hangs from, in tile widths.
Seated by its tab alone the piece comes up flush against the edge and reads as cut into the
tile rather than laid against it; this backs it off by a third of the tab's length, which
still leaves the tab well inside the notch.
-}
connectorStandoff :: Double
connectorStandoff = 0.022

{- | The sides of a threshold tile in the order they run round it, which is the order
its icons are printed in: from due left, turning the way the screen does. Where it
starts is arbitrary, but it is fixed, so the engine works out the same order again when
it turns the tile and the icons stay with the sides they are drawn on.
-}
aroundFrom :: (Double, Double) -> [(a, (Double, Double))] -> [a]
aroundFrom (hx, hy) = map fst . sortOn (turning . snd)
 where
  turning (x, y) = let a = atan2 (y - hy) (x - hx) in if a < pi then a + 2 * pi else a

{- | Where a corner piece stands in the junction these tiles share, and which space
of each tile its edges meet, in the order its printed icons run round it. The piece is
laid against the first tile named and reaches the rest from there.

The junction is where the three tiles' centres average out. On a regular lattice that is
also the point equidistant from all three of them, so a piece standing there meets each
tile alike -- which is where the tabletop version puts it, and the piece is then centred
rather than laid against any one tile.

The tiles' centres and their spaces' spots are passed in rather than read off the map,
so this answers for a piece set out at setup and for one that has since walked round a
tile (Secrets of the Order card 135).
-}
cornerSeat
  :: [(NeighborhoodId, (Double, Double))]
  -> (SpaceId -> Maybe (Double, Double))
  -> CornerSeat
cornerSeat ts spotOf =
  CornerSeat
    { spot = seat
    , angle = laidAgainst
    , faces = aroundFrom seat [(sid, spot sid) | sid <- faces]
    }
 where
  centres = map snd ts
  seat = meanOf centres
  {- Which way round it is laid. A corner piece is a three-armed junction, and an arm has
  to point at the tile it is laid against. At an angle of zero its arms point due left and
  a third of a turn either side of that, so bringing the first of them onto the anchor is
  a half turn back from the anchor's own direction. Setup and card 135 then turn it a
  further whole number of thirds, which keeps every arm on a tile and only changes which
  icon is on which.

  This has to be worked out afresh at every corner: the direction of the tile it is laid
  against turns by a sixth as the piece walks round, so a piece that keeps the angle it
  was first laid at points its arms between the tiles instead of at them. -}
  laidAgainst = case centres of
    [] -> 0
    (ax, ay) : _ -> let (sx, sy) = seat in atan2 (ay - sy) (ax - sx) * 180 / pi - 180
  faces = [facingSpace (tile nid) c seat | (nid, c) <- ts]
  spot sid = fromMaybe (0, 0) (spotOf sid)
  meanOf [] = (0, 0)
  meanOf ps =
    ( sum (map fst ps) / fromIntegral (length ps)
    , sum (map snd ps) / fromIntegral (length ps)
    )

-- | The space of a tile at this centre whose wedge points nearest the given place.
facingSpace :: TileDef -> (Double, Double) -> (Double, Double) -> SpaceId
facingSpace t (x, y) (tx, ty) =
  let want = atan2 (ty - y) (tx - x)
      off slot = abs (atan2 (sin (slotAngle t.variety slot - want)) (cos (slotAngle t.variety slot - want)))
   in spaceIdFor (slotName t (minimumBy (comparing off) [A, B, C]))

{- | The tiles that ring a tile, in the order they run round it on the screen. Two of
them that fall next to each other in this order share a corner with the tile in the
middle, which is how a piece walks from one of its corners to the next.
-}
ringAround :: (Double, Double) -> [(NeighborhoodId, (Double, Double))] -> [NeighborhoodId]
ringAround here = aroundFrom here . filter (touching . snd)
 where
  touching (x, y) = let d = dist (x, y) in d > 1e-9 && d < 2 * (0.5 + streetLength)
  dist (x, y) = sqrt ((x - fst here) ** 2 + (y - snd here) ** 2)

{- | Where a corner piece stands, which way round it is laid, and the spaces its edges
meet, in the order its printed icons run round it.
-}
data CornerSeat = CornerSeat
  { spot :: (Double, Double)
  , angle :: Double
  , faces :: [SpaceId]
  }

-- | How far apart two unconnected clusters of tiles are set out.
clusterGap :: Double
clusterGap = 2

{- | Where each tile sits. A scenario's tiles need not form one connected group --
several Under Dark Waves maps are two separate clusters joined only by travel
routes -- so each cluster is laid out from its own origin and then shifted clear
of the ones already placed, the way they would be set out on the table.
-}
placeTiles
  :: [NeighborhoodId]
  -> [StreetDef]
  -> [ThresholdTile]
  -> [ClusterLink]
  -> [(NeighborhoodId, (Double, Double))]
placeTiles nids streets thresholds clusters = foldl cluster [] nids
 where
  links =
    concat [[(s.from, s.edge, s.to), (s.to, opposite s.edge, s.from)] | s <- streets]
      <> concat [[(t.from, t.edge, t.to), (t.to, opposite t.edge, t.from)] | t <- thresholds]
      <> concat [[(c.from, c.edge, c.to), (c.to, opposite c.edge, c.from)] | c <- clusters]
  cluster placed n
    | n `elem` map fst placed = placed
    | otherwise =
        let here = go [(n, (0, 0))] [n]
            shift = case placed of
              [] -> 0
              _ -> maximum [x | (_, (x, _)) <- placed] + clusterGap - minimum [x | (_, (x, _)) <- here]
         in placed <> [(k, (x + shift, y)) | (k, (x, y)) <- here]
  go placed [] = placed
  go placed (n : queue) =
    let (x, y) = fromJustNote "placed" (lookup n placed)
        new =
          [ (dest, (x + d * cos a, y + d * sin a))
          | (src, e, dest) <- links
          , src == n
          , dest `notElem` map fst placed
          , let a = edgeAngle e
                d = 2 * edgeApothem e + streetLength
          ]
        fresh = foldl (\acc (k, v) -> if k `elem` map fst acc then acc else acc <> [(k, v)]) [] new
     in go (placed <> fresh) (queue <> map fst fresh)

slotName :: TileDef -> Slot -> Text
slotName t s = let (a, b, c) = t.spaces in case s of A -> a; B -> b; C -> c

edgeSpaces :: TileDef -> Edge -> [SpaceId]
edgeSpaces t e = map (spaceIdFor . slotName t) (edgeSlots t.variety e)

tileSpaces :: TileDef -> [SpaceDef]
tileSpaces t =
  [ SpaceDef (spaceIdFor n) n LocationSpace (Just t.neighborhood)
  | n <- let (a, b, c) = t.spaces in [a, b, c]
  ]

buildMap :: [NeighborhoodId] -> [StreetDef] -> MapDef
buildMap nids streets = buildMapWith nids streets [] []

-- | 'buildMap', with the travel routes and mysteries that hang off a tile's edges.
buildMapWith :: [NeighborhoodId] -> [StreetDef] -> [RouteDef] -> [MysteryTile] -> MapDef
buildMapWith nids streets = buildMapLaidOut nids streets []

{- | 'buildMapWith', told where the clusters of tiles no street reaches are set
out. Without a link a cluster is simply set down clear of the ones already
placed; with one it is laid against the edge it names, still joined by nothing.
-}
buildMapLaidOut
  :: [NeighborhoodId] -> [StreetDef] -> [ClusterLink] -> [RouteDef] -> [MysteryTile] -> MapDef
buildMapLaidOut nids streets clusters routes mysteries =
  buildMapOf nids streets noPieces {routes, mysteries, clusters}

-- | 'buildMap' with every other kind of piece a sheet may lay out.
buildMapOf :: [NeighborhoodId] -> [StreetDef] -> Pieces -> MapDef
buildMapOf nids streets pieces =
  MapDef
    { neighborhoods = [NeighborhoodDef t.neighborhood t.name t.town (tileSpaces t) | t <- ts]
    , otherSpaces =
        [SpaceDef (streetId s) (streetName s) (StreetSpace s.streetType) Nothing | s <- streets]
          <> [SpaceDef (routeId r) (routeName r) (TravelRouteSpace r.routeType) Nothing | r <- routes]
          <> [SpaceDef (spaceIdFor m.name) m.name MysterySpace (Just m.from) | m <- mysteries]
          <> [ SpaceDef
                 (thresholdId t.thresholdType)
                 (thresholdName t.thresholdType)
                 (ThresholdSpace t.thresholdType)
                 Nothing
             | t <- thresholds
             ]
          <> [ SpaceDef
                 (thresholdId c.thresholdType)
                 (thresholdName c.thresholdType)
                 (ThresholdSpace c.thresholdType)
                 Nothing
             | c <- corners
             ]
    , borders =
        internal
          <> concatMap streetBorders streets
          <> concat [hangingBorders (routeId r) r.from r.edge | r <- routes]
          <> concat [hangingBorders (spaceIdFor m.name) m.from m.edge | m <- mysteries]
          <> concatMap thresholdBorders thresholds
          <> concatMap cornerBorders corners
    , layout =
        BoardLayout tilePlacements (streetPlacements <> hangingPlacements <> thresholdPlacements) anchors
    }
 where
  Pieces {routes, mysteries, thresholds, corners, clusters, nudges} = pieces
  -- a scenario lays out at most one tile of each kind, so the type names the space
  thresholdId = spaceIdFor . thresholdName
  -- laid between two hexes, a threshold borders both edges the way a street does
  thresholdBorders t =
    [ (thresholdId t.thresholdType, sid, h)
    | (side, h) <- zipHazards t.hazards (aroundFrom (thresholdSpot t) (map withSpot (sides t)))
    , sid <- side
    ]
  sides t = [edgeSpaces (tile t.from) t.edge, edgeSpaces (tile t.to) (opposite t.edge)]
  thresholdSpot t = let ((x1, y1), (x2, y2)) = (pos t.from, pos t.to) in ((x1 + x2) / 2, (y1 + y2) / 2)
  withSpot side = (side, meanSpot side)
  meanSpot side = mean (mapMaybe (`lookup` spaceSpots) side)
  spaceSpots = [(a.space, (a.x, a.y)) | a <- anchors]
  {- A corner tile abuts one space of each hex it touches: the space whose wedge faces
  the corner, which is where the centres of those hexes average out. -}
  cornerBorders c =
    [ (thresholdId c.thresholdType, sid, h)
    | (sid, h) <- zipHazards c.hazards (seatOf c).faces
    ]
  seatOf c = cornerSeat [(nid, pos nid) | nid <- c.tiles] (\sid -> lookup sid spaceSpots)
  cornerAt c = (seatOf c).spot
  mean [] = (0, 0)
  mean ps =
    ( sum (map fst ps) / fromIntegral (length ps)
    , sum (map snd ps) / fromIntegral (length ps)
    )
  thresholdPlacements =
    [ StreetPlacement (thresholdId t.thresholdType) ((x1 + x2) / 2) ((y1 + y2) / 2) (edgeDegrees t.edge)
    | t <- thresholds
    , let (x1, y1) = pos t.from
          (x2, y2) = pos t.to
    ]
      -- a corner piece stands in the junction itself, turned to face what it is laid against
      <> [ StreetPlacement (thresholdId c.thresholdType) x y (seatOf c).angle
         | c <- corners
         , let (x, y) = cornerAt c
         ]
  routeId r = SpaceId (coerce r.from <> "--" <> routeSlug r.routeType)
  routeName r = (tile r.from).name <> " – " <> routeLabel r.routeType
  -- a dangling space borders only the tile spaces along the edge it is attached to
  hangingBorders sid nid e = [(sid, s, Nothing) | s <- edgeSpaces (tile nid) e]
  -- it sits where a street to a neighbour would have sat, just outside that edge
  hangingPlacements =
    [ StreetPlacement sid (x + reach * cos a) (y + reach * sin a) (edgeDegrees e)
    | (sid, nid, e) <-
        [(routeId r, r.from, r.edge) | r <- routes]
          <> [(spaceIdFor m.name, m.from, m.edge) | m <- mysteries]
    , let (x, y) = pos nid
          a = edgeAngle e
          reach = edgeApothem e + connectorDepth * (0.5 - connectorTab) + connectorStandoff
    ]
  {- The tiles sit on a regular hex lattice, so every neighbour is the same distance away
  and every junction between three of them is the same shape. The rows used to be spread a
  little on top of that, which made the diagonal junctions a different shape from the
  straight ones -- and that is what a corner piece could not fit. -}
  positions = [(nid, shift nid p) | (nid, p) <- placeTiles nids streets thresholds clusters]
  shift nid (x, y) = case lookup nid nudges of
    Nothing -> (x, y)
    Just (dx, dy) -> (x + dx, y + dy)
  pos nid = fromJustNote ("tile not connected to the map: " <> show nid) (lookup nid positions)
  tilePlacements = [TilePlacement nid x y | (nid, (x, y)) <- positions]
  streetPlacements =
    [ StreetPlacement (streetId s) ((x1 + x2) / 2) ((y1 + y2) / 2) (atan2 (y2 - y1) (x2 - x1) * 180 / pi)
    | s <- streets
    , let (x1, y1) = pos s.from
          (x2, y2) = pos s.to
    ]
  anchors =
    [ SpaceAnchor sid (x + anchorRadius * cos a) (y + anchorRadius * sin a)
    | t <- ts
    , let (x, y) = pos t.neighborhood
    , (slot, sid) <- zip [A, B, C] (map (.id) (tileSpaces t))
    , let a = slotAngle t.variety slot
    ]
  ts = map tile nids
  internal =
    concat
      [ [ (a.id, b.id, hazardBetween t a.name b.name)
        | (i, a) <- zip [0 :: Int ..] sps
        , (j, b) <- zip [0 ..] sps
        , i < j
        ]
      | t <- ts
      , let sps = tileSpaces t
      ]
  hazardBetween t a b =
    listToMaybe [h | (x, y, h) <- t.hazards, (x, y) == (a, b) || (x, y) == (b, a)]
  streetId s = SpaceId (coerce s.from <> "--" <> coerce s.to)
  streetName s = (tile s.from).name <> " – " <> (tile s.to).name <> " street"
  streetBorders s =
    [ (streetId s, sid, Nothing)
    | sid <- edgeSpaces (tile s.from) s.edge <> edgeSpaces (tile s.to) (opposite s.edge)
    ]

{- | A threshold tile's icons against its sides, in the order the tile prints them.
Which icon ends up facing which tile is settled when the tile is laid down, so setup
turns it from here; this only has to hand them out as printed.
-}
zipHazards :: [Hazard] -> [a] -> [(a, Maybe Hazard)]
zipHazards hs sides = zip sides (map Just hs <> repeat Nothing)

thresholdName :: ThresholdType -> Text
thresholdName = \case
  HiddenPath -> "Hidden Path"
  DerelictPortal -> "Derelict Portal"
  WildGateway -> "Wild Gateway"

routeSlug :: RouteType -> Text
routeSlug = \case
  CountryRoad -> "country-road"
  FerryTerminal -> "ferry-terminal"
  TrainPlatform -> "train-platform"

routeLabel :: RouteType -> Text
routeLabel = \case
  CountryRoad -> "country road"
  FerryTerminal -> "ferry terminal"
  TrainPlatform -> "train platform"

edgeDegrees :: Edge -> Double
edgeDegrees e = edgeAngle e * 180 / pi

{- | A piece of map a card puts into play part way through a scenario, laid
against a tile already on the board. That tile sits at the origin here, so the
engine has only to shift the piece onto wherever it already stands.
-}
addedMap :: NeighborhoodId -> Edge -> [NeighborhoodId] -> [StreetDef] -> [RouteDef] -> MapDef
addedMap against edge nids streets routes =
  addedMapOf against edge nids streets noPieces {routes}

-- | 'addedMap' with every other kind of piece the card lays down beside the tiles.
addedMapOf :: NeighborhoodId -> Edge -> [NeighborhoodId] -> [StreetDef] -> Pieces -> MapDef
addedMapOf against edge nids streets pieces = case nids of
  [] -> buildMap [against] []
  lead : _ ->
    buildMapOf
      (against : nids)
      streets
      pieces {clusters = ClusterLink against edge lead : pieces.clusters}
