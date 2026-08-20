module Arkham.Location.CardDefs.TheMidwinterGala where

import Arkham.Location.CardDefs.Import

artGalleryTheMidwinterGala :: CardDef
artGalleryTheMidwinterGala =
  locationWithUnrevealed
    "71009"
    "Ground-Floor Room"
    [Manor, GroundFloor]
    Moon
    []
    "Art Gallery"
    [Manor, GroundFloor]
    Triangle
    [Diamond, Spade, Moon, Square]
    TheMidwinterGala

ballroomTheMidwinterGala :: CardDef
ballroomTheMidwinterGala =
  locationWithUnrevealed
    "71010"
    "Ground-Floor Room"
    [Manor, GroundFloor]
    Moon
    []
    "Ballroom"
    [Manor, GroundFloor]
    Square
    [Diamond, Spade, Triangle, Moon]
    TheMidwinterGala

barroom :: CardDef
barroom =
  locationWithUnrevealed
    "71011"
    "Ground-Floor Room"
    [Manor, GroundFloor]
    Moon
    []
    "Barroom"
    [Manor, GroundFloor]
    Spade
    [Diamond, Moon, Triangle, Square]
    TheMidwinterGala

bedroomTheMidwinterGala :: CardDef
bedroomTheMidwinterGala =
  locationWithUnrevealed
    "71012"
    "Second-Floor Room"
    [Manor, Private, SecondFloor]
    Heart
    []
    "Bedroom"
    [Manor, Private, SecondFloor]
    T
    [Circle, Hourglass, Heart]
    TheMidwinterGala

lanternChamber :: CardDef
lanternChamber =
  victory 1
    $ location "71008" "Lantern Chamber" [Manor, Private, Basement] Star [Diamond] TheMidwinterGala

libraryTheMidwinterGala :: CardDef
libraryTheMidwinterGala =
  victory 1
    $ locationWithUnrevealed
      "71013"
      "Second-Floor Room"
      [Manor, Private, SecondFloor]
      Heart
      []
      "Library"
      [Manor, Private, SecondFloor]
      Hourglass
      [Circle, T, Heart]
      TheMidwinterGala

lobby :: CardDef
lobby =
  location
    "71007"
    "Lobby"
    [Manor, GroundFloor]
    Diamond
    [Moon, Spade, Triangle, Square, Star]
    TheMidwinterGala

parlorTheMidwinterGala :: CardDef
parlorTheMidwinterGala =
  victory 1
    $ locationWithUnrevealed
      "71014"
      "Second-Floor Room"
      [Manor, Private, SecondFloor]
      Heart
      []
      "Parlor"
      [Manor, Private, SecondFloor]
      Circle
      [T, Hourglass, Heart]
      TheMidwinterGala
