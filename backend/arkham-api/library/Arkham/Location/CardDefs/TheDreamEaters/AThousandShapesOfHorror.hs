{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDreamEaters.AThousandShapesOfHorror where

import Arkham.Location.CardDefs.Import

attic :: CardDef
attic =
  victory 1
    $ location
      "06181"
      "Attic"
      mempty
      Circle
      [Heart]
      AThousandShapesOfHorror

burialGround :: CardDef
burialGround =
  victory 1
    $ location
      "06174"
      "Burial Ground"
      [Graveyard]
      Moon
      [Square, Plus]
      AThousandShapesOfHorror

downstairsDoorwayDen :: CardDef
downstairsDoorwayDen =
  victory 1
    $ locationWithUnrevealed
      "06176"
      "Downstairs Doorway"
      mempty
      Hourglass
      [Square]
      "Den"
      mempty
      Hourglass
      [Square]
      AThousandShapesOfHorror

downstairsDoorwayParlor :: CardDef
downstairsDoorwayParlor =
  victory 1
    $ locationWithUnrevealed
      "06177"
      "Downstairs Doorway"
      mempty
      Hourglass
      [Square]
      "Parlor"
      mempty
      Hourglass
      [Square]
      AThousandShapesOfHorror

frontPorchEntryway :: CardDef
frontPorchEntryway =
  locationWithUnrevealed
    "06175"
    "Front Porch"
    mempty
    Square
    [Moon, Heart, Hourglass]
    "Entryway"
    mempty
    Square
    [Moon, Heart, Hourglass]
    AThousandShapesOfHorror

mysteriousStairs_183 :: CardDef
mysteriousStairs_183 =
  location
    "06183"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_184 :: CardDef
mysteriousStairs_184 =
  location
    "06184"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_185 :: CardDef
mysteriousStairs_185 =
  location
    "06185"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_186 :: CardDef
mysteriousStairs_186 =
  location
    "06186"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_187 :: CardDef
mysteriousStairs_187 =
  location
    "06187"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_188 :: CardDef
mysteriousStairs_188 =
  location
    "06188"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

unmarkedTomb :: CardDef
unmarkedTomb =
  victory 1
    $ location
      "06182"
      "Unmarked Tomb"
      [Graveyard]
      Plus
      [Moon]
      AThousandShapesOfHorror

upstairsDoorwayBedroom :: CardDef
upstairsDoorwayBedroom =
  victory 1
    $ locationWithUnrevealed
      "06180"
      "Upstairs Doorway"
      mempty
      Diamond
      [Heart]
      "Bedroom"
      mempty
      Diamond
      [Heart]
      AThousandShapesOfHorror

upstairsDoorwayLibrary :: CardDef
upstairsDoorwayLibrary =
  victory 1
    $ locationWithUnrevealed
      "06179"
      "Upstairs Doorway"
      mempty
      Diamond
      [Heart]
      "Library"
      mempty
      Diamond
      [Heart]
      AThousandShapesOfHorror

upstairsHallway :: CardDef
upstairsHallway =
  location
    "06178"
    "Upstairs Hallway"
    mempty
    Heart
    [Diamond, Square, Circle]
    AThousandShapesOfHorror
