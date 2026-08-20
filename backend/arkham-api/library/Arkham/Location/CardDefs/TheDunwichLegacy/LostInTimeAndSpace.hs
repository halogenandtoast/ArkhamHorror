module Arkham.Location.CardDefs.TheDunwichLegacy.LostInTimeAndSpace where

import Arkham.Keyword qualified as Keyword
import Arkham.Location.CardDefs.Import

anotherDimension :: CardDef
anotherDimension =
  location
    "02320"
    ("Another Dimension" <:> "Unfettered by Reality")
    [Otherworld]
    Circle
    [Square, Diamond, Triangle]
    LostInTimeAndSpace

dimensionalDoorway :: CardDef
dimensionalDoorway =
  singleSided
    $ location
      "02328"
      "Dimensional Doorway"
      [Otherworld, Extradimensional]
      Squiggle
      [Triangle, Moon]
      LostInTimeAndSpace

endlessBridge :: CardDef
endlessBridge =
  singleSided
    $ ( location
          "02326"
          "Endless Bridge"
          [Otherworld, Extradimensional]
          Triangle
          [Square, Squiggle]
          LostInTimeAndSpace
      )
      { cdEncounterSetQuantity = Just 2
      }

prismaticCascade :: CardDef
prismaticCascade =
  singleSided
    $ ( location
          "02325"
          "Prismatic Cascade"
          [Otherworld, Extradimensional]
          Diamond
          [Square, Plus]
          LostInTimeAndSpace
      )
      { cdEncounterSetQuantity = Just 2
      }

stepsOfYhagharl :: CardDef
stepsOfYhagharl =
  singleSided
    $ location
      "02327"
      "Steps of Y'hagharl"
      [Otherworld, Extradimensional]
      Plus
      [Diamond, Moon]
      LostInTimeAndSpace

tearThroughSpace :: CardDef
tearThroughSpace =
  singleSided
    $ ( location
          "02324"
          "Tear Through Space"
          [Otherworld, Extradimensional]
          Square
          [Diamond, Triangle, Square]
          LostInTimeAndSpace
      )
      { cdKeywords = setFromList [Keyword.Surge]
      , cdEncounterSetQuantity = Just 4
      }

tearThroughTime :: CardDef
tearThroughTime =
  location
    "02322"
    "Tear Through Time"
    [Otherworld]
    Moon
    [Circle, Plus, Squiggle]
    LostInTimeAndSpace

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  location
    "02321"
    "The Edge of the Universe"
    [Otherworld]
    Moon
    [Plus, Squiggle]
    LostInTimeAndSpace
