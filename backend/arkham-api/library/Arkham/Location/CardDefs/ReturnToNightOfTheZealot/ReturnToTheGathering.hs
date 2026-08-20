module Arkham.Location.CardDefs.ReturnToNightOfTheZealot.ReturnToTheGathering where

import Arkham.Location.CardDefs.Import

bathroom :: CardDef
bathroom = location "50016" "Bathroom" mempty Star [T] ReturnToTheGathering

bedroom :: CardDef
bedroom = location "50015" "Bedroom" mempty Heart [T] ReturnToTheGathering

deepBelowYourHouse :: CardDef
deepBelowYourHouse =
  victory 1
    $ locationWithUnrevealed
      "50021"
      "Deep Below Your House"
      mempty
      Squiggle
      [Plus]
      "Ghoul Pits"
      mempty
      Squiggle
      [Plus]
      ReturnToTheGathering

farAboveYourHouse :: CardDef
farAboveYourHouse =
  victory 1
    $ locationWithUnrevealed
      "50019"
      "Far Above Your House"
      mempty
      Moon
      [Triangle]
      "Field of Graves"
      mempty
      Moon
      [Triangle]
      ReturnToTheGathering

guestHall :: CardDef
guestHall =
  location
    "50014"
    "Guest Hall"
    mempty
    T
    [Circle, Heart, Star, Square]
    ReturnToTheGathering

holeInTheWall :: CardDef
holeInTheWall =
  locationWithUnrevealed
    "50017"
    "Hole in the Wall"
    mempty
    Square
    [T]
    "Hallway"
    mempty
    Square
    [T, Triangle, Plus, Diamond]
    ReturnToTheGathering

returnToAttic :: CardDef
returnToAttic =
  locationWithUnrevealed
    "50018"
    "Attic"
    mempty
    Triangle
    [Square]
    "Attic"
    mempty
    Triangle
    [Square, Moon]
    ReturnToTheGathering

returnToCellar :: CardDef
returnToCellar =
  locationWithUnrevealed
    "50020"
    "Cellar"
    mempty
    Plus
    [Square]
    "Cellar"
    mempty
    Plus
    [Square, Squiggle]
    ReturnToTheGathering

studyAberrantGateway :: CardDef
studyAberrantGateway =
  location
    "50013"
    ("Study" <:> "Aberrant Gateway")
    mempty
    Circle
    [T]
    ReturnToTheGathering
