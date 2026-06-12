module Arkham.Location.CardDefs.RelicsOfThePast where

import Arkham.Location.CardDefs.Import

secretPassageRelicsOfThePast :: CardDef
secretPassageRelicsOfThePast =
  victory 1
    $ singleSided
    $ location
      "90070"
      "Secret Passage"
      [Ancient, Ruins]
      Squiggle
      [Diamond, Triangle, Hourglass]
      RelicsOfThePast

innerChamber :: CardDef
innerChamber =
  victory 1
    $ location "90071" "Inner Chamber" [Forgotten, Ruins] Hourglass [Squiggle] RelicsOfThePast

ancientHallRelicsOfThePast :: CardDef
ancientHallRelicsOfThePast =
  singleSided
    $ location
      "90072"
      "Ancient Hall"
      [Ancient, Ruins]
      Square
      [Circle, Star, Diamond]
      RelicsOfThePast
