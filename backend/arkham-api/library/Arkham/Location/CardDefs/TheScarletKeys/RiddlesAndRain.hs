module Arkham.Location.CardDefs.TheScarletKeys.RiddlesAndRain where

import Arkham.Location.CardDefs.Import

bigBen :: CardDef
bigBen =
  victory 1
    $ location
      "09511"
      "Big Ben"
      [London]
      Triangle
      [Equals, Circle]
      RiddlesAndRain

kensingtonGardens :: CardDef
kensingtonGardens =
  victory 1
    $ location
      "09513"
      "Kensington Gardens"
      [London]
      Square
      [Equals]
      RiddlesAndRain

rainyLondonStreets :: CardDef
rainyLondonStreets =
  location
    "09510"
    "Rainy London Streets"
    [London]
    Equals
    [Circle, Square, Triangle, Squiggle]
    RiddlesAndRain

theTowerBridge :: CardDef
theTowerBridge =
  location
    "09514"
    "The Tower Bridge"
    [London]
    Squiggle
    [Equals, Moon, T]
    RiddlesAndRain

towerOfLondon :: CardDef
towerOfLondon =
  location
    "09516"
    "Tower of London"
    [London]
    Moon
    [Squiggle, T, Hourglass]
    RiddlesAndRain

towerPrison :: CardDef
towerPrison =
  victory 1
    $ location
      "09517"
      "Tower Prison"
      [London]
      Hourglass
      [Moon]
      RiddlesAndRain

traitorsGate :: CardDef
traitorsGate =
  location
    "09515"
    "Traitor's Gate"
    [London]
    T
    [Squiggle, Moon]
    RiddlesAndRain

westminsterAbbey :: CardDef
westminsterAbbey =
  location
    "09512"
    "Westminster Abbey"
    [London]
    Circle
    [Equals, Triangle]
    RiddlesAndRain
