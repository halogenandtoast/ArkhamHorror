module Arkham.Location.CardDefs.ThePathToCarcosa.TheLastKing where

import Arkham.Location.CardDefs.Import

ballroom :: CardDef
ballroom =
  location "03071" "Ballroom" mempty Square [T, Circle, Squiggle] TheLastKing

courtyard :: CardDef
courtyard =
  location
    "03074"
    "Courtyard"
    mempty
    Circle
    [Squiggle, Square, T, Equals, Plus]
    TheLastKing

diningRoom :: CardDef
diningRoom =
  location "03075" "Dining Room" mempty Squiggle [Square, Circle] TheLastKing

foyer :: CardDef
foyer = location "03070" "Foyer" mempty T [Circle, Square, Equals] TheLastKing

gallery :: CardDef
gallery = location "03073" "Gallery" mempty Plus [Equals, Circle] TheLastKing

livingRoom :: CardDef
livingRoom =
  location "03072" "Living Room" mempty Equals [T, Circle, Plus] TheLastKing
