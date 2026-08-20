module Arkham.Location.CardDefs.TheForgottenAge.Rainforest where

import Arkham.Location.CardDefs.Import

circuitousTrail :: CardDef
circuitousTrail =
  victory 1
    $ singleSided
    $ location
      "04073"
      "Circuitous Trail"
      [Jungle]
      Heart
      [Hourglass, Diamond, Moon, T]
      Rainforest

overgrownRuins :: CardDef
overgrownRuins =
  victory 2
    $ singleSided
    $ location
      "04075"
      "Overgrown Ruins"
      [Ancient, Ruins]
      T
      [Moon, Heart, Equals]
      Rainforest

pathOfThorns :: CardDef
pathOfThorns =
  singleSided
    $ location
      "04069"
      "Path of Thorns"
      [Jungle]
      Square
      [Circle, Diamond, Triangle, Squiggle]
      Rainforest

riverCanyon :: CardDef
riverCanyon =
  singleSided
    $ location
      "04070"
      "River Canyon"
      [Jungle]
      Diamond
      [Circle, Moon, Heart, Triangle, Square]
      Rainforest

ropeBridge :: CardDef
ropeBridge =
  singleSided
    $ location
      "04071"
      "Rope Bridge"
      [Jungle]
      Moon
      [Circle, Diamond, Heart, T]
      Rainforest

serpentsHaven :: CardDef
serpentsHaven =
  victory 1
    $ singleSided
    $ location
      "04072"
      "Serpent's Haven"
      [Jungle]
      Triangle
      [Squiggle, Square, Diamond, Hourglass]
      Rainforest

templeOfTheFang :: CardDef
templeOfTheFang =
  victory 2
    $ singleSided
    $ location
      "04074"
      "Temple of the Fang"
      [Ancient, Ruins]
      Squiggle
      [Square, Triangle, Equals]
      Rainforest
