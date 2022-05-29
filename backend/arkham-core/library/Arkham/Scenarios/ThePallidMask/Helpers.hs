module Arkham.Scenarios.ThePallidMask.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Label
import Arkham.Classes.Entity
import Arkham.Message hiding (Label)
import Arkham.Direction
import Arkham.Location.Attrs

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

positionToLabel :: (Int, Int) -> Label
positionToLabel (x, y) = Label . pack $ "pos" <> fromI x <> fromI y
 where
  fromI n
    | n < 10 = "0" <> show n
    | otherwise = show n


placeAtDirection :: Direction -> LocationAttrs -> Card -> [Message]
placeAtDirection direction attrs card =
  [ PlaceLocation card
  , SetLocationLabel
    (toLocationId card)
    (unLabel $ positionToLabel newPos)
  , PlacedLocationDirection (toLocationId card) direction (toId attrs)
  ]
 where
   (x, y) = posLabelToPosition (mkLabel $ locationLabel attrs)
   newPos = case direction of
              Above -> (x, y + 1)
              Below -> (x, y - 1)
              LeftOf -> (x - 1, y)
              RightOf -> (x + 1, y)
