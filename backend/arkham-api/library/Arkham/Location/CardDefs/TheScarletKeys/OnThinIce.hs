module Arkham.Location.CardDefs.TheScarletKeys.OnThinIce where

import Arkham.Location.CardDefs.Import

anchorage :: CardDef
anchorage =
  location
    "09617"
    "Anchorage"
    []
    Squiggle
    [Diamond]
    OnThinIce

condemnedGoldMine :: CardDef
condemnedGoldMine =
  locationWithUnrevealed
    "09624"
    "Outer Wilderness"
    [Wilderness]
    T
    [Square]
    "Condemned Gold Mine"
    []
    Hourglass
    [Square]
    OnThinIce

fairbanks :: CardDef
fairbanks =
  location
    "09618"
    "Fairbanks"
    []
    Diamond
    [Squiggle, Triangle, Square, Circle, Moon]
    OnThinIce

forgottenOutpost :: CardDef
forgottenOutpost =
  locationWithUnrevealed
    "09622"
    "Outer Wilderness"
    [Wilderness]
    T
    [Square]
    "Forgotten Outpost"
    [Wilderness]
    Spade
    [Square, Moon]
    OnThinIce

frozenLake :: CardDef
frozenLake =
  locationWithUnrevealed
    "09620"
    "Alaskan Wilderness"
    [Wilderness]
    Triangle
    [Diamond]
    "Frozen Lake"
    [Wilderness]
    Circle
    [Diamond]
    OnThinIce

huntersLodge :: CardDef
huntersLodge =
  victory 1
    $ locationWithUnrevealed
      "09623"
      "Outer Wilderness"
      [Wilderness]
      T
      [Square]
      "Hunter's Lodge"
      [Wilderness]
      Heart
      [Square]
      OnThinIce

isolatedRoad :: CardDef
isolatedRoad =
  victory 1
    $ locationWithUnrevealed
      "09621"
      "Alaskan Wilderness"
      [Wilderness]
      Triangle
      [Diamond]
      "Isolated Road"
      [Wilderness]
      Moon
      [Diamond, Spade]
      OnThinIce

mountainStream :: CardDef
mountainStream =
  locationWithUnrevealed
    "09619"
    "Alaskan Wilderness"
    [Wilderness]
    Triangle
    [Diamond]
    "Mountain Stream"
    [Wilderness]
    Square
    [Diamond, T, Spade, Heart, Hourglass]
    OnThinIce

outsidersLair :: CardDef
outsidersLair =
  otherSideIs "09615"
    $ victory 1
    $ location
      "09615b"
      "Outsiders' Lair"
      [Otherworld]
      NoSymbol
      []
      OnThinIce
