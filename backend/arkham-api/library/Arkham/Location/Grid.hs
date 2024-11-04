module Arkham.Location.Grid where

import Arkham.Prelude hiding ((<|))

import Arkham.Direction
import Arkham.Id
import Arkham.Layout
import Data.Sequence ((<|), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Records
import Text.Printf

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

data Pos = Pos Int Int
  deriving stock (Eq, Show, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

positionRow :: Pos -> Int
positionRow (Pos _ y) = y

positionColumn :: Pos -> Int
positionColumn (Pos x _) = x

instance HasField "row" Pos Int where
  getField = positionRow

instance HasField "column" Pos Int where
  getField = positionColumn

data GridLocation = GridLocation Pos LocationId
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

gridLocationToPosition :: GridLocation -> Pos
gridLocationToPosition (GridLocation pos _) = pos

setGridLocationPosition :: Pos -> GridLocation -> GridLocation
setGridLocationPosition pos (GridLocation _ b) = GridLocation pos b

data GridRow
  = GridRow
      (Seq (Maybe GridLocation))
      (Maybe GridLocation)
      (Seq (Maybe GridLocation))
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

initGrid :: Grid
initGrid = Grid (Seq.singleton initRow) initRow (Seq.singleton initRow)
 where
  initRow = GridRow (Seq.singleton Nothing) Nothing (Seq.singleton Nothing)

gridRowLeft :: GridRow -> Int
gridRowLeft (GridRow left _ _) = Seq.length left

gridRowRight :: GridRow -> Int
gridRowRight (GridRow _ _ right) = Seq.length right

data Grid = Grid
  { gridAbove :: Seq GridRow
  , gridCenter :: GridRow
  , gridBelow :: Seq GridRow
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- First check do we need to extend grid in a direction, if so extend
-- Then find the correct row to insert
-- Then find the correct column to insert and change Nothing to Just
insertGrid :: GridLocation -> Grid -> Grid
insertGrid loc c =
  let pos = gridLocationToPosition loc
      c' = extendGridForPosition pos c
   in setGrid (Just loc) pos c'

viewGrid :: Pos -> Grid -> Maybe GridLocation
viewGrid (Pos x y) (Grid above center below) =
  case compare y 0 of
    LT -> maybe Nothing viewGridRow (Seq.lookup (abs y - 1) below)
    GT -> maybe Nothing viewGridRow (Seq.lookup (Seq.length above - y) above)
    EQ -> viewGridRow center
 where
  viewGridRow (GridRow left middle right) =
    case compare x 0 of
      LT -> join $ Seq.lookup (Seq.length left + x) left
      GT -> join $ Seq.lookup (x - 1) right
      EQ -> middle

-- We always want the cosmos 1 bigger than the total space needed in each
-- direction to have empty positions
extendGridForPosition :: Pos -> Grid -> Grid
extendGridForPosition (Pos x y) c =
  case compare y 0 of
    LT ->
      let c' = if abs y >= belowAmount then nTimes (abs y + 1 - belowAmount) extendGridDown c else c
       in go c'
    GT ->
      let c' = if y >= aboveAmount then nTimes (y + 1 - aboveAmount) extendGridUp c else c
       in go c'
    EQ -> go c
 where
  aboveAmount = gridAboveAmount c
  belowAmount = gridBelowAmount c
  leftAmount = gridLeftAmount c
  rightAmount = gridRightAmount c
  go c' = case compare x 0 of
    LT ->
      if abs x >= leftAmount
        then nTimes (abs x + 1 - leftAmount) extendGridLeft c'
        else c'
    GT ->
      if x >= rightAmount
        then nTimes (x + 1 - rightAmount) extendGridRight c'
        else c'
    EQ -> c'

setGridRow :: Int -> Maybe GridLocation -> GridRow -> GridRow
setGridRow x mloc (GridRow left center right) =
  case compare x 0 of
    LT -> GridRow (Seq.adjust (const mloc) (Seq.length left + x) left) center right
    GT -> GridRow left center (Seq.adjust (const mloc) (x - 1) right)
    EQ -> GridRow left mloc right

setGrid :: Maybe GridLocation -> Pos -> Grid -> Grid
setGrid mloc pos@(Pos x y) (Grid above center below) = case compare y 0 of
  LT ->
    Grid
      above
      center
      (Seq.adjust (setGridRow x $ fmap (setGridLocationPosition pos) mloc) (abs y - 1) below)
  GT ->
    Grid
      ( Seq.adjust
          (setGridRow x $ fmap (setGridLocationPosition pos) mloc)
          (Seq.length above - y)
          above
      )
      center
      below
  EQ -> Grid above (setGridRow x (fmap (setGridLocationPosition pos) mloc) center) below

gridLeftAmount :: Grid -> Int
gridLeftAmount (Grid _ center _) = gridRowLeft center

gridRightAmount :: Grid -> Int
gridRightAmount (Grid _ center _) = gridRowRight center

gridAboveAmount :: Grid -> Int
gridAboveAmount (Grid above _ _) = Seq.length above

gridBelowAmount :: Grid -> Int
gridBelowAmount (Grid _ _ below) = Seq.length below

extendRowLeft :: GridRow -> GridRow
extendRowLeft (GridRow left center right) =
  GridRow (Nothing <| left) center right

extendRowRight :: GridRow -> GridRow
extendRowRight (GridRow left center right) =
  GridRow left center (right |> Nothing)

extendGridLeft :: Grid -> Grid
extendGridLeft (Grid above center below) =
  Grid (fmap extendRowLeft above) (extendRowLeft center) (fmap extendRowLeft below)

extendGridRight :: Grid -> Grid
extendGridRight (Grid above center below) =
  Grid (fmap extendRowRight above) (extendRowRight center) (fmap extendRowRight below)

extendGridUp :: Grid -> Grid
extendGridUp c@(Grid above center below) =
  Grid
    ( (GridRow (Seq.replicate leftAmount Nothing) Nothing (Seq.replicate rightAmount Nothing)) <| above
    )
    center
    below
 where
  leftAmount = gridLeftAmount c
  rightAmount = gridRightAmount c

extendGridDown :: Grid -> Grid
extendGridDown c@(Grid above center below) =
  Grid
    above
    center
    ( below
        |> (GridRow (Seq.replicate leftAmount Nothing) Nothing (Seq.replicate rightAmount Nothing))
    )
 where
  leftAmount = gridLeftAmount c
  rightAmount = gridRightAmount c

gridToTemplate :: Grid -> [GridTemplateRow]
gridToTemplate c =
  let leftBy = gridLeftAmount c
      rightBy = gridRightAmount c
      aboveBy = gridAboveAmount c
      belowBy = gridBelowAmount c
      xRange = [-leftBy .. rightBy]
      yRange = reverse [-belowBy .. aboveBy]
   in [GridTemplateRow $ T.unwords [gridLabel (Pos x y) | x <- xRange] | y <- yRange]

gridRowToTemplate :: GridRow -> GridTemplateRow
gridRowToTemplate (GridRow left center right) =
  GridTemplateRow
    . T.unwords
    $ toList (fmap (maybe "." gridCellToTemplate) left)
    <> [maybe "." gridCellToTemplate center]
    <> toList (fmap (maybe "." gridCellToTemplate) right)

gridCellToTemplate :: GridLocation -> Text
gridCellToTemplate (GridLocation pos _) = gridLabel pos

clearGrid :: Pos -> Grid -> Grid
clearGrid (Pos x y) (Grid above center below) =
  case compare y 0 of
    LT -> Grid above center (Seq.adjust (setGridRow x Nothing) (abs y - 1) below)
    GT ->
      Grid (Seq.adjust (setGridRow x Nothing) (Seq.length above - y) above) center below
    EQ -> Grid above (setGridRow x Nothing center) below

isEmpty :: Pos -> Grid -> Bool
isEmpty pos grid = isNothing $ viewGrid pos grid

updatePosition :: Pos -> GridDirection -> Pos
updatePosition (Pos x y) dir = case dir of
  GridLeft -> Pos (x - 1) y
  GridRight -> Pos (x + 1) y
  GridUp -> Pos x (y + 1)
  GridDown -> Pos x (y - 1)

adjacentPositions :: Pos -> [Pos]
adjacentPositions pos = map (updatePosition pos) [GridLeft, GridRight, GridUp, GridDown]

positionsInDirections :: Pos -> [GridDirection] -> [Pos]
positionsInDirections pos dirs = map (updatePosition pos) dirs

emptyPositionsInDirections :: Grid -> Pos -> [GridDirection] -> [Pos]
emptyPositionsInDirections grid pos dirs = filter (`isEmpty` grid) $ positionsInDirections pos dirs

gridLabel :: Pos -> Text
gridLabel (Pos x y) =
  T.pack
    $ printf "pos%s%02d%s%02d" (negativeStr x) (abs x) (negativeStr y) (abs y)
 where
  negativeStr :: Int -> String
  negativeStr n = if n < 0 then "n" else ""

findInGrid :: LocationId -> Grid -> Maybe Pos
findInGrid b c =
  let leftBy = gridLeftAmount c
      rightBy = gridRightAmount c
      aboveBy = gridAboveAmount c
      belowBy = gridBelowAmount c
      xRange = [-leftBy .. rightBy]
      yRange = [-belowBy .. aboveBy]
      positions = [Pos x y | x <- xRange, y <- yRange]
   in flip find positions $ \pos -> case viewGrid pos c of
        Just (GridLocation _ b') -> b == b'
        _ -> False

flattenGrid :: Grid -> [GridLocation]
flattenGrid (Grid above center below) =
  concat (toList (fmap flattenGridRow above))
    <> flattenGridRow center
    <> concat (toList (fmap flattenGridRow below))

flattenGridRow :: GridRow -> [GridLocation]
flattenGridRow (GridRow left center right) = catMaybes $ toList left <> [center] <> toList right
