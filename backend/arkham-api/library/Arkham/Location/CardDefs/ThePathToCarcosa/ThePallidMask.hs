module Arkham.Location.CardDefs.ThePathToCarcosa.ThePallidMask where

import Arkham.Location.CardDefs.Import

blockedPassage :: CardDef
blockedPassage =
  locationWithUnrevealed
    "03256"
    "Catacombs"
    []
    NoSymbol
    []
    "Blocked Passage"
    []
    NoSymbol
    []
    ThePallidMask

boneFilledCaverns :: CardDef
boneFilledCaverns =
  victory 1
    $ locationWithUnrevealed
      "03250"
      "Catacombs"
      []
      NoSymbol
      []
      "Bone-Filled Caverns"
      []
      NoSymbol
      []
      ThePallidMask

candlelitTunnels :: CardDef
candlelitTunnels =
  quantity 2
    $ locationWithUnrevealed
      "03252"
      "Catacombs"
      []
      NoSymbol
      []
      "Candlelit Tunnels"
      []
      NoSymbol
      []
      ThePallidMask

cryptOfTheSepulchralLamp :: CardDef
cryptOfTheSepulchralLamp =
  locationWithUnrevealed
    "03249"
    "Catacombs"
    []
    NoSymbol
    []
    "Crypt of the Sepulchral Lamp"
    []
    NoSymbol
    []
    ThePallidMask

labyrinthOfBones :: CardDef
labyrinthOfBones =
  quantity 2
    $ locationWithUnrevealed
      "03253"
      "Catacombs"
      []
      NoSymbol
      []
      "Labyrinth of Bones"
      []
      NoSymbol
      []
      ThePallidMask

narrowShaft :: CardDef
narrowShaft =
  victory 1
    $ locationWithUnrevealed
      "03254"
      "Catacombs"
      []
      NoSymbol
      []
      "Narrow Shaft"
      []
      NoSymbol
      []
      ThePallidMask

shiveringPools :: CardDef
shiveringPools =
  victory 1
    $ locationWithUnrevealed
      "03255"
      "Catacombs"
      []
      NoSymbol
      []
      "Shivering Pools"
      []
      NoSymbol
      []
      ThePallidMask

stoneArchways :: CardDef
stoneArchways =
  quantity 2
    $ locationWithUnrevealed
      "03248"
      "Catacombs"
      []
      NoSymbol
      []
      "Stone Archways"
      []
      NoSymbol
      []
      ThePallidMask

theGateToHell :: CardDef
theGateToHell =
  locationWithUnrevealed
    "03247"
    "Catacombs"
    []
    NoSymbol
    []
    "The Gate to Hell"
    []
    NoSymbol
    []
    ThePallidMask

tombOfShadows :: CardDef
tombOfShadows =
  victory 1
    $ locationWithUnrevealed
      "03257"
      "Catacombs"
      []
      NoSymbol
      []
      "Tomb of Shadows"
      []
      NoSymbol
      []
      ThePallidMask

wellOfSouls :: CardDef
wellOfSouls =
  victory 1
    $ locationWithUnrevealed
      "03251"
      "Catacombs"
      []
      NoSymbol
      []
      "Well of Souls"
      []
      NoSymbol
      []
      ThePallidMask
