module Arkham.Scenarios.ThePallidMask.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Direction
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Message hiding (Label)
import Arkham.Id
import Arkham.Label
import Arkham.Location.Types
import Arkham.Matcher
import Control.Monad (zipWithM)

posLabelToPosition :: Label -> (Int, Int)
posLabelToPosition lbl = case drop 3 (unpack . unLabel $ lbl) of
  (x10 : x1 : y10 : y1 : []) -> (toI x10 * 10 + toI x1, toI y10 * 10 + toI y1)
  _ -> error "Invalid position label"
 where
  toI = \case
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _ -> error "not a digit"

startPosition :: (Int, Int)
startPosition = (2, 6)

getStartingLocation :: HasGame m => m LocationId
getStartingLocation = selectJust $ LocationWithLabel $ positionToLabel startPosition

positionToLabel :: (Int, Int) -> Label
positionToLabel (x, y) = Label . pack $ "pos" <> fromI x <> fromI y
 where
  fromI n
    | n < 10 = "0" <> show n
    | otherwise = show n

placeAtDirection :: Direction -> LocationAttrs -> GameT (Card -> GameT [Message])
placeAtDirection direction attrs = do
  -- we need to determine what we are connected to based on our pos, the only way to do this is to get locations with labels
  let placedPosition = newPos direction (posLabelToPosition . mkLabel $ locationLabel attrs)

  mLeftLocation <- selectOne $ LocationWithLabel $ positionToLabel $ newPos LeftOf placedPosition
  mRightLocation <- selectOne $ LocationWithLabel $ positionToLabel $ newPos RightOf placedPosition
  mAboveLocation <- selectOne $ LocationWithLabel $ positionToLabel $ newPos Above placedPosition
  mBelowLocation <- selectOne $ LocationWithLabel $ positionToLabel $ newPos Below placedPosition

  pure $ \card -> do
    (locationId, placement) <- placeLocation card
    pure
      $ [ placement
        , SetLocationLabel locationId (unLabel $ positionToLabel placedPosition)
        ]
      <> case mLeftLocation of
        Just lid -> [PlacedLocationDirection locationId LeftOf lid]
        Nothing -> []
      <> case mRightLocation of
        Just lid -> [PlacedLocationDirection locationId RightOf lid]
        Nothing -> []
      <> case mAboveLocation of
        Just lid -> [PlacedLocationDirection locationId Above lid]
        Nothing -> []
      <> case mBelowLocation of
        Just lid -> [PlacedLocationDirection locationId Below lid]
        Nothing -> []
 where
  newPos dir (x, y) = case dir of
    Above -> (x, y + 1)
    Below -> (x, y - 1)
    LeftOf -> (x - 1, y)
    RightOf -> (x + 1, y)

directionEmpty :: HasGame m => LocationAttrs -> Direction -> m Bool
directionEmpty attrs dir = selectNone $ LocationInDirection dir (LocationWithId $ toId attrs)

toMaybePlacement :: LocationAttrs -> Direction -> GameT (Maybe (Card -> GameT [Message]))
toMaybePlacement attrs dir = do
  isEmpty <- directionEmpty attrs dir
  if isEmpty
    then Just <$> placeAtDirection dir attrs
    else pure Nothing

placeDrawnLocations :: LocationAttrs -> [Card] -> [Direction] -> GameT ()
placeDrawnLocations attrs cards directions = do
  placements <- mapMaybeM (toMaybePlacement attrs) directions
  msgs <- concat <$> zipWithM ($) placements cards
  pushAll msgs
