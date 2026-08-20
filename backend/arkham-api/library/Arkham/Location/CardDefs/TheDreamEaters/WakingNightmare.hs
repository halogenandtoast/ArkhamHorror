module Arkham.Location.CardDefs.TheDreamEaters.WakingNightmare where

import Arkham.Location.CardDefs.Import

emergencyRoom :: CardDef
emergencyRoom =
  victory 1
    $ location
      "06071"
      "Emergency Room"
      [StMarys]
      Square
      [Circle, Triangle]
      WakingNightmare

experimentalTherapiesWard :: CardDef
experimentalTherapiesWard =
  victory 2
    $ location
      "06072"
      "Experimental Therapies Ward"
      [StMarys]
      Triangle
      [Circle, Square, Heart]
      WakingNightmare

morgue :: CardDef
morgue =
  victory 2
    $ locationWithUnrevealed
      "06075"
      "Basement Door"
      [StMarys, Basement]
      Plus
      [Heart]
      "Morgue"
      [StMarys, Basement]
      Hourglass
      [Heart]
      WakingNightmare

operatingRoom :: CardDef
operatingRoom =
  victory 2
    $ locationWithUnrevealed
      "06076"
      "Basement Door"
      [StMarys, Basement]
      Plus
      [Heart]
      "Operating Room"
      [StMarys, Basement]
      T
      [Heart]
      WakingNightmare

privateRoom :: CardDef
privateRoom =
  locationWithUnrevealed
    "06077"
    "Basement Door"
    [StMarys, Basement]
    Plus
    [Heart]
    "Private Room"
    [StMarys, Basement]
    Moon
    [Heart]
    WakingNightmare

recordsOffice :: CardDef
recordsOffice =
  victory 2
    $ location
      "06073"
      "Records Office"
      [StMarys]
      Diamond
      [Circle]
      WakingNightmare

stairwell :: CardDef
stairwell =
  victory 1
    $ location
      "06074"
      "Stairwell"
      [StMarys]
      Heart
      [Triangle, Plus, Hourglass, T, Moon]
      WakingNightmare

waitingRoom :: CardDef
waitingRoom =
  location
    "06070"
    "Waiting Room"
    [StMarys]
    Circle
    [Diamond, Triangle, Square]
    WakingNightmare
