module Arkham.Location.CardDefs.TheForgottenAge.ReturnToRainforest where

import Arkham.Location.CardDefs.Import

cloudForest :: CardDef
cloudForest =
  victory 1
    $ singleSided
    $ location
      "53070"
      "Cloud Forest"
      [Jungle]
      Heart
      [Hourglass, Diamond, Moon, T]
      ReturnToRainforest

riversideTemple :: CardDef
riversideTemple =
  singleSided
    $ location
      "53067"
      "Riverside Temple"
      [Ancient, Jungle]
      Square
      [Circle, Diamond, Triangle, Squiggle]
      ReturnToRainforest

trailOfTheDead :: CardDef
trailOfTheDead =
  victory 1
    $ singleSided
    $ location
      "53069"
      "Trail of the Dead"
      [Jungle]
      Triangle
      [Squiggle, Square, Diamond, Hourglass]
      ReturnToRainforest

waterfall :: CardDef
waterfall =
  singleSided
    $ location
      "53068"
      "Waterfall"
      [Jungle]
      Moon
      [Circle, Diamond, Heart, T]
      ReturnToRainforest
