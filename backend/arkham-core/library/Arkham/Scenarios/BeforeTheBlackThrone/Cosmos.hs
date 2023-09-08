module Arkham.Scenarios.BeforeTheBlackThrone.Cosmos where

import Arkham.Prelude hiding ((<|))

import Arkham.Scenario.Types
import Data.Sequence ((<|), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Text.Printf

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

data GridDirection = GridUp | GridDown | GridLeft | GridRight

data Pos = Pos Int Int
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Show Pos where
  show (Pos x y) = printf "pos%02d%02d" x y

data CosmosLocation a b = EmptySpace Pos a | CosmosLocation Pos b
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

cosmosLocationToPosition :: CosmosLocation a b -> Pos
cosmosLocationToPosition (EmptySpace pos _) = pos
cosmosLocationToPosition (CosmosLocation pos _) = pos

data CosmosRow a b
  = CosmosRow
      (Seq (Maybe (CosmosLocation a b)))
      (Maybe (CosmosLocation a b))
      (Seq (Maybe (CosmosLocation a b)))
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initCosmos :: Cosmos a b
initCosmos = Cosmos mempty (CosmosRow mempty Nothing mempty) mempty

cosmosRowLeft :: CosmosRow a b -> Int
cosmosRowLeft (CosmosRow left _ _) = Seq.length left

cosmosRowRight :: CosmosRow a b -> Int
cosmosRowRight (CosmosRow _ _ right) = Seq.length right

data Cosmos a b = Cosmos
  { cosmosAbove :: Seq (CosmosRow a b)
  , cosmosCenter :: CosmosRow a b
  , cosmosBelow :: Seq (CosmosRow a b)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- First check do we need to extend cosmos in a direction, if so extend
-- Then find the correct row to insert
-- Then find the correct column to insert and change Nothing to Just
insertCosmos :: CosmosLocation a b -> Cosmos a b -> Cosmos a b
insertCosmos loc c =
  let pos = cosmosLocationToPosition loc
      c' = extendCosmosForPosition pos c
   in setCosmos (Just loc) pos c'

viewCosmos :: Pos -> Cosmos a b -> Maybe (CosmosLocation a b)
viewCosmos (Pos x y) (Cosmos above center below) =
  case compare y 0 of
    LT -> maybe Nothing viewCosmosRow (Seq.lookup (abs y - 1) below)
    GT -> maybe Nothing viewCosmosRow (Seq.lookup (Seq.length above - y) above)
    EQ -> viewCosmosRow center
 where
  viewCosmosRow (CosmosRow left middle right) =
    case compare x 0 of
      LT -> join $ Seq.lookup (Seq.length left + x) left
      GT -> join $ Seq.lookup (x - 1) right
      EQ -> middle

extendCosmosForPosition :: Pos -> Cosmos a b -> Cosmos a b
extendCosmosForPosition (Pos x y) c =
  case compare y 0 of
    LT ->
      let c' = if abs y > belowAmount then nTimes (abs y - belowAmount) extendCosmosDown c else c
       in go c'
    GT ->
      let c' = if y > aboveAmount then nTimes (y - aboveAmount) extendCosmosUp c else c
       in go c'
    EQ -> go c
 where
  aboveAmount = cosmosAboveAmount c
  belowAmount = cosmosBelowAmount c
  leftAmount = cosmosLeftAmount c
  rightAmount = cosmosRightAmount c
  go c' = case compare x 0 of
    LT ->
      if abs x > leftAmount
        then nTimes (abs x - leftAmount) extendCosmosLeft c'
        else c'
    GT ->
      if x > rightAmount
        then nTimes (x - rightAmount) extendCosmosRight c'
        else c'
    EQ -> c'

setCosmosRow :: Int -> Maybe (CosmosLocation a b) -> CosmosRow a b -> CosmosRow a b
setCosmosRow x mloc (CosmosRow left center right) =
  case compare x 0 of
    LT -> CosmosRow (Seq.adjust (const mloc) (Seq.length left + x) left) center right
    GT -> CosmosRow left center (Seq.adjust (const mloc) (x - 1) right)
    EQ -> CosmosRow left mloc right

setCosmos :: Maybe (CosmosLocation a b) -> Pos -> Cosmos a b -> Cosmos a b
setCosmos mloc (Pos x y) (Cosmos above center below) = case compare y 0 of
  LT -> Cosmos above center (Seq.adjust (setCosmosRow x mloc) (abs y - 1) below)
  GT ->
    Cosmos (Seq.adjust (setCosmosRow x mloc) (Seq.length above - y) above) center below
  EQ -> Cosmos above (setCosmosRow x mloc center) below

cosmosLeftAmount :: Cosmos a b -> Int
cosmosLeftAmount (Cosmos _ center _) = cosmosRowLeft center

cosmosRightAmount :: Cosmos a b -> Int
cosmosRightAmount (Cosmos _ center _) = cosmosRowRight center

cosmosAboveAmount :: Cosmos a b -> Int
cosmosAboveAmount (Cosmos above _ _) = Seq.length above

cosmosBelowAmount :: Cosmos a b -> Int
cosmosBelowAmount (Cosmos _ _ below) = Seq.length below

extendRowLeft :: CosmosRow a b -> CosmosRow a b
extendRowLeft (CosmosRow left center right) =
  CosmosRow (Nothing <| left) center right

extendRowRight :: CosmosRow a b -> CosmosRow a b
extendRowRight (CosmosRow left center right) =
  CosmosRow left center (right |> Nothing)

extendCosmosLeft :: Cosmos a b -> Cosmos a b
extendCosmosLeft (Cosmos above center below) =
  Cosmos (fmap extendRowLeft above) (extendRowLeft center) (fmap extendRowLeft below)

extendCosmosRight :: Cosmos a b -> Cosmos a b
extendCosmosRight (Cosmos above center below) =
  Cosmos (fmap extendRowRight above) (extendRowRight center) (fmap extendRowRight below)

extendCosmosUp :: Cosmos a b -> Cosmos a b
extendCosmosUp c@(Cosmos above center below) =
  Cosmos
    ( above
        |> (CosmosRow (Seq.replicate leftAmount Nothing) Nothing (Seq.replicate rightAmount Nothing))
    )
    center
    below
 where
  leftAmount = cosmosLeftAmount c
  rightAmount = cosmosRightAmount c

extendCosmosDown :: Cosmos a b -> Cosmos a b
extendCosmosDown c@(Cosmos above center below) =
  Cosmos
    above
    center
    ( below
        |> (CosmosRow (Seq.replicate leftAmount Nothing) Nothing (Seq.replicate rightAmount Nothing))
    )
 where
  leftAmount = cosmosLeftAmount c
  rightAmount = cosmosRightAmount c

cosmosToGrid :: Cosmos a b -> [GridTemplateRow]
cosmosToGrid (Cosmos above center below) =
  toList $ fmap cosmosRowToGrid ((above |> center) <> below)

cosmosRowToGrid :: CosmosRow a b -> GridTemplateRow
cosmosRowToGrid (CosmosRow left center right) =
  GridTemplateRow
    . T.unwords
    $ toList (fmap (maybe "." cosmosCellToGrid) left)
      <> [maybe "." cosmosCellToGrid center]
      <> toList (fmap (maybe "." cosmosCellToGrid) right)

cosmosCellToGrid :: CosmosLocation a b -> Text
cosmosCellToGrid (EmptySpace pos _) = tshow pos
cosmosCellToGrid (CosmosLocation pos _) = tshow pos

clearCosmos :: Pos -> Cosmos a b -> Cosmos a b
clearCosmos (Pos x y) (Cosmos above center below) =
  case compare y 0 of
    LT -> Cosmos above center (Seq.adjust (setCosmosRow x Nothing) (abs y - 1) below)
    GT ->
      Cosmos (Seq.adjust (setCosmosRow x Nothing) (Seq.length above - y) above) center below
    EQ -> Cosmos above (setCosmosRow x Nothing center) below

isEmpty :: Maybe (CosmosLocation a b) -> Bool
isEmpty Nothing = True
isEmpty (Just (EmptySpace _ _)) = True
isEmpty (Just (CosmosLocation _ _)) = False

updatePosition :: Pos -> GridDirection -> Pos
updatePosition (Pos x y) dir = case dir of
  GridLeft -> Pos (x - 1) y
  GridRight -> Pos (x + 1) y
  GridUp -> Pos x (y + 1)
  GridDown -> Pos x (y - 1)

{- | Slide a cosmos location and fill in the empty space with replacement
This errors because we want to make sure we handle this correctly, the
caller should know whether or not a slide is possible
-}
slide :: Pos -> GridDirection -> Maybe (CosmosLocation a b) -> Cosmos a b -> Cosmos a b
slide p dir replacement c =
  let newPos = updatePosition p dir
      current = viewCosmos p c
      target = viewCosmos newPos c
   in if isEmpty target
        then setCosmos current newPos (setCosmos replacement p c)
        else error "Tried to slide in invalid location"
