module AH3e.Content.Tiles (
  Variety (..),
  Edge (..),
  TileDef (..),
  StreetDef (..),
  tiles,
  tile,
  spaceIdFor,
  edgeSpaces,
  buildMap,
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

placeTiles :: [NeighborhoodId] -> [StreetDef] -> [(NeighborhoodId, (Double, Double))]
placeTiles [] _ = []
placeTiles (origin : _) streets = go [(origin, (0, 0))] [origin]
 where
  links = concat [[(s.from, s.edge, s.to), (s.to, opposite s.edge, s.from)] | s <- streets]
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
buildMap nids streets =
  MapDef
    { neighborhoods = [NeighborhoodDef t.neighborhood t.name t.town (tileSpaces t) | t <- ts]
    , otherSpaces =
        [SpaceDef (streetId s) (streetName s) (StreetSpace s.streetType) Nothing | s <- streets]
    , borders = internal <> concatMap streetBorders streets
    , layout = BoardLayout tilePlacements streetPlacements anchors
    }
 where
  positions = [(nid, (x, y * rowStretch)) | (nid, (x, y)) <- placeTiles nids streets]
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
