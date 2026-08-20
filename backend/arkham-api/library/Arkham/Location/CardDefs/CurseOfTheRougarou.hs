module Arkham.Location.CardDefs.CurseOfTheRougarou where

import Arkham.Location.CardDefs.Import

audubonPark :: CardDef
audubonPark =
  victory 1
    $ locationWithUnrevealed
      "81011"
      "Riverside"
      [Riverside]
      Squiggle
      [Triangle, Squiggle]
      "Audubon Park"
      [Riverside]
      Squiggle
      [Triangle, Squiggle]
      TheBayou

brackishWaters :: CardDef
brackishWaters =
  location
    "81010"
    "Brackish Waters"
    [Riverside, Bayou]
    Triangle
    [Squiggle, Square, Diamond, Hourglass]
    TheBayou

broadmoor :: CardDef
broadmoor =
  victory 1
    $ locationWithUnrevealed
      "81009"
      "New Orleans"
      [NewOrleans]
      Plus
      [Square, Plus]
      "Broadmoor"
      [NewOrleans]
      Plus
      [Square, Plus]
      TheBayou

cursedShores :: CardDef
cursedShores =
  location
    "81007"
    "Cursed Shores"
    [NewOrleans, Bayou]
    Square
    [Plus, Triangle, Diamond, Hourglass]
    TheBayou

faubourgMarigny :: CardDef
faubourgMarigny =
  locationWithUnrevealed
    "81012"
    "Riverside"
    [Riverside]
    Squiggle
    [Triangle, Squiggle]
    "Faubourg Marigny"
    [Riverside]
    Squiggle
    [Triangle, Squiggle]
    TheBayou

forgottenMarsh :: CardDef
forgottenMarsh =
  location
    "81013"
    "Forgotten Marsh"
    [Wilderness, Bayou]
    Diamond
    [Moon, Square, Triangle, Hourglass]
    TheBayou

foulSwamp :: CardDef
foulSwamp =
  location
    "81016"
    "Foul Swamp"
    [Unhallowed, Bayou]
    Hourglass
    [Equals, Square, Triangle, Diamond]
    TheBayou

gardenDistrict :: CardDef
gardenDistrict =
  locationWithUnrevealed
    "81008"
    "New Orleans"
    [NewOrleans]
    Plus
    [Square, Plus]
    "Garden District"
    [NewOrleans]
    Plus
    [Square, Plus]
    TheBayou

overgrownCairns :: CardDef
overgrownCairns =
  locationWithUnrevealed
    "81018"
    "Unhallowed Land"
    [Unhallowed]
    Equals
    [Hourglass, Equals]
    "Overgrown Cairns"
    [Unhallowed]
    Equals
    [Hourglass, Equals]
    TheBayou

ritualGrounds :: CardDef
ritualGrounds =
  victory 1
    $ locationWithUnrevealed
      "81017"
      "Unhallowed Land"
      [Unhallowed]
      Equals
      [Hourglass, Equals]
      "Ritual Grounds"
      [Unhallowed]
      Equals
      [Hourglass, Equals]
      TheBayou

trappersCabin :: CardDef
trappersCabin =
  locationWithUnrevealed
    "81014"
    "Wilderness"
    [Wilderness]
    Moon
    [Diamond, Moon]
    "Trapper's Cabin"
    [Wilderness]
    Moon
    [Diamond, Moon]
    TheBayou

twistedUnderbrush :: CardDef
twistedUnderbrush =
  victory 1
    $ locationWithUnrevealed
      "81015"
      "Wilderness"
      [Wilderness]
      Moon
      [Diamond, Moon]
      "Twisted Underbrush"
      [Wilderness]
      Moon
      [Diamond, Moon]
      TheBayou
