module Arkham.Location.CardDefs.TheForgottenAge.TheDoomOfEztli where

import Arkham.Location.CardDefs.Import

ancientHall :: CardDef
ancientHall =
  singleSided
    $ location "04063" "Ancient Hall" [Ancient, Ruins] Square [Circle, Star, Diamond] TheDoomOfEztli

burialPit :: CardDef
burialPit =
  victory 1
    $ singleSided
    $ location "04065" "Burial Pit" [Ancient, Ruins] Triangle [Star, Diamond, Squiggle] TheDoomOfEztli

chamberOfTime :: CardDef
chamberOfTime =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "04068"
      "Chamber of Time"
      [Forgotten, Ruins]
      Hourglass
      [Squiggle]
      TheDoomOfEztli

entryway :: CardDef
entryway = location "04060" "Entryway" [Ancient, Ruins] Circle [Square, Star] TheDoomOfEztli

grandChamber :: CardDef
grandChamber =
  victory 1
    $ singleSided
    $ location "04064" "Grand Chamber" [Ancient, Ruins] Star [Circle, Square, Triangle] TheDoomOfEztli

secretPassage :: CardDef
secretPassage =
  victory 1
    $ singleSided
    $ location
      "04067"
      "Secret Passage"
      [Ancient, Ruins]
      Squiggle
      [Diamond, Triangle, Hourglass]
      TheDoomOfEztli

undergroundRuins :: CardDef
undergroundRuins =
  vengeance 1
    $ singleSided
    $ location
      "04066"
      "Underground Ruins"
      [Ancient, Ruins]
      Diamond
      [Square, Triangle, Squiggle]
      TheDoomOfEztli
