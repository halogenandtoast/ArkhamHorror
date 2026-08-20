module Arkham.Location.CardDefs.TheDunwichLegacy.WhereDoomAwaits where

import Arkham.Location.CardDefs.Import

aTearInThePath :: CardDef
aTearInThePath =
  locationWithUnrevealed
    "02290"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "A Tear in the Path"
    [Dunwich, Woods, Altered]
    Equals
    [Square, Squiggle]
    WhereDoomAwaits

ascendingPath :: CardDef
ascendingPath =
  location
    "02283"
    "Ascending Path"
    [Dunwich, SentinelHill]
    Square
    [Triangle, Diamond, T, Equals, Moon]
    WhereDoomAwaits

baseOfTheHill :: CardDef
baseOfTheHill =
  location
    "02282"
    "Base of the Hill"
    [Dunwich, SentinelHill]
    Triangle
    [Square, Plus, Squiggle, Hourglass]
    WhereDoomAwaits

destroyedPath :: CardDef
destroyedPath =
  locationWithUnrevealed
    "02287"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Destroyed Path"
    [Dunwich, Woods]
    Squiggle
    [Triangle, Equals]
    WhereDoomAwaits

dimensionalGap :: CardDef
dimensionalGap =
  locationWithUnrevealed
    "02289"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Dimensional Gap"
    [Dunwich, Woods, Altered]
    T
    [Square, Moon]
    WhereDoomAwaits

eerieGlade :: CardDef
eerieGlade =
  locationWithUnrevealed
    "02286"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Eerie Glade"
    [Dunwich, Woods]
    Hourglass
    [Triangle, Plus]
    WhereDoomAwaits

frozenSpring :: CardDef
frozenSpring =
  locationWithUnrevealed
    "02288"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Frozen Spring"
    [Dunwich, Woods]
    Plus
    [Triangle, Hourglass]
    WhereDoomAwaits

lostMemories :: CardDef
lostMemories =
  locationWithUnrevealed
    "02292"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Lost Memories"
    [Dunwich, Woods, Altered]
    T
    [Square, Moon]
    WhereDoomAwaits

sentinelPeak :: CardDef
sentinelPeak =
  victory 2
    $ location
      "02284"
      "Sentinel Peak"
      [Dunwich, SentinelHill]
      Diamond
      [Square]
      WhereDoomAwaits

slaughteredWoods :: CardDef
slaughteredWoods =
  locationWithUnrevealed
    "02285"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Slaughtered Woods"
    [Dunwich, Woods]
    Plus
    [Triangle, Hourglass]
    WhereDoomAwaits

uprootedWoods :: CardDef
uprootedWoods =
  locationWithUnrevealed
    "02291"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Uprooted Woods"
    [Dunwich, Woods, Altered]
    Moon
    [Square, T]
    WhereDoomAwaits
