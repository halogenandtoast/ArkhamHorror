module AH3e.Content.Tiles (
  Variety (..),
  Edge (..),
  TileDef (..),
  StreetDef (..),
  RouteDef (..),
  MysteryTile (..),
  ClusterLink (..),
  tiles,
  tile,
  spaceIdFor,
  edgeSpaces,
  buildMap,
  buildMapWith,
  buildMapLaidOut,
  addedMap,
) where

import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Ids
import Data.Char (isAlphaNum, toLower)
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

{- | Where a cluster of tiles no street reaches is set out: the edge of an already
placed tile it is laid against. Nothing connects along it -- it only says where
the tiles go, the way they would be put down on the table.
-}
data ClusterLink = ClusterLink {from :: NeighborhoodId, edge :: Edge, to :: NeighborhoodId}

slug :: Text -> Text
slug =
  T.intercalate "-"
    . T.words
    . T.map (\c -> if isAlphaNum c then toLower c else ' ')
    . T.filter (/= '\'')

spaceIdFor :: Text -> SpaceId
spaceIdFor = SpaceId . slug

mkTile :: Text -> Town -> Variety -> (Text, Text, Text) -> TileDef
mkTile n = TileDef (NeighborhoodId (slug n)) n

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
    mkTile "The Underworld" OtherWorld V2 ("City of the Gugs", "Vale of Pnath", "Vaults of Zin")
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

-- extra vertical room between rows so diagonal streets clear the lower tiles
rowStretch :: Double
rowStretch = 1.05

streetLength, anchorRadius :: Double
streetLength = 0.37
anchorRadius = 0.3

{- | How deep a connector is drawn and how much of that depth is the tab it joins
by, both as a fraction of the tile's width (the frontend's @CONNECTOR_W@ over the
art's own proportions comes to the first). A connector is not street sized, so it
is seated by its own depth rather than in the slot a street would take: its
shoulder lands on the tile's edge and the tab reaches into the notch.
-}
connectorDepth, connectorTab :: Double
connectorDepth = 0.38
connectorTab = 0.2

-- | How far apart two unconnected clusters of tiles are set out.
clusterGap :: Double
clusterGap = 2

{- | Where each tile sits. A scenario's tiles need not form one connected group --
several Under Dark Waves maps are two separate clusters joined only by travel
routes -- so each cluster is laid out from its own origin and then shifted clear
of the ones already placed, the way they would be set out on the table.
-}
placeTiles
  :: [NeighborhoodId] -> [StreetDef] -> [ClusterLink] -> [(NeighborhoodId, (Double, Double))]
placeTiles nids streets clusters = foldl cluster [] nids
 where
  links =
    concat [[(s.from, s.edge, s.to), (s.to, opposite s.edge, s.from)] | s <- streets]
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
  MapDef
    { neighborhoods = [NeighborhoodDef t.neighborhood t.name t.town (tileSpaces t) | t <- ts]
    , otherSpaces =
        [SpaceDef (streetId s) (streetName s) (StreetSpace s.streetType) Nothing | s <- streets]
          <> [SpaceDef (routeId r) (routeName r) (TravelRouteSpace r.routeType) Nothing | r <- routes]
          <> [SpaceDef (spaceIdFor m.name) m.name MysterySpace (Just m.from) | m <- mysteries]
    , borders =
        internal
          <> concatMap streetBorders streets
          <> concat [hangingBorders (routeId r) r.from r.edge | r <- routes]
          <> concat [hangingBorders (spaceIdFor m.name) m.from m.edge | m <- mysteries]
    , layout = BoardLayout tilePlacements (streetPlacements <> hangingPlacements) anchors
    }
 where
  routeId r = SpaceId (coerce r.from <> "--" <> routeSlug r.routeType)
  routeName r = (tile r.from).name <> " – " <> routeLabel r.routeType
  -- a dangling space borders only the tile spaces along the edge it is attached to
  hangingBorders sid nid e = [(sid, s, Nothing) | s <- edgeSpaces (tile nid) e]
  -- it sits where a street to a neighbour would have sat, just outside that edge
  hangingPlacements =
    [ StreetPlacement sid (x + reach * cos a) (y + reach * sin a * rowStretch) (edgeDegrees e)
    | (sid, nid, e) <-
        [(routeId r, r.from, r.edge) | r <- routes]
          <> [(spaceIdFor m.name, m.from, m.edge) | m <- mysteries]
    , let (x, y) = pos nid
          a = edgeAngle e
          reach = edgeApothem e + connectorDepth * (0.5 - connectorTab)
    ]
  positions = [(nid, (x, y * rowStretch)) | (nid, (x, y)) <- placeTiles nids streets clusters]
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
      [ [(a.id, b.id, Nothing) | (i, a) <- zip [0 :: Int ..] sps, (j, b) <- zip [0 ..] sps, i < j]
      | t <- ts
      , let sps = tileSpaces t
      ]
  streetId s = SpaceId (coerce s.from <> "--" <> coerce s.to)
  streetName s = (tile s.from).name <> " – " <> (tile s.to).name <> " street"
  streetBorders s =
    [ (streetId s, sid, Nothing)
    | sid <- edgeSpaces (tile s.from) s.edge <> edgeSpaces (tile s.to) (opposite s.edge)
    ]

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
addedMap against edge nids streets routes = case nids of
  [] -> buildMap [against] []
  lead : _ -> buildMapLaidOut (against : nids) streets [ClusterLink against edge lead] routes []
