module Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons where

import Arkham.Location.CardDefs.Import

factoryFloorWestDay :: CardDef
factoryFloorWestDay =
  location
    "13039"
    ("Factory Floor" <:> "West")
    [Warehouse, Day]
    Star
    [Triangle, T, Square]
    NewHorizons

factoryFloorEastDay :: CardDef
factoryFloorEastDay =
  location
    "13040"
    ("Factory Floor" <:> "East")
    [Warehouse, Day]
    Triangle
    [Star, T, Moon]
    NewHorizons

managersOfficeDay :: CardDef
managersOfficeDay =
  victory 1
    $ location "13041" "Manager's Office" [Warehouse, Day] Square [T, Star] NewHorizons

loadingDockDay :: CardDef
loadingDockDay =
  location "13042" "Loading Dock" [Warehouse, Day] T [Square, Star, Triangle, Moon] NewHorizons

storageDay :: CardDef
storageDay =
  victory 1
    $ location "13043" "Storage" [Warehouse, Day] Moon [T, Triangle, Hourglass] NewHorizons

factoryFloorWestNight :: CardDef
factoryFloorWestNight =
  location
    "13044"
    ("Factory Floor" <:> "West")
    [Warehouse, Night]
    Star
    [Triangle, T, Square]
    NewHorizons

factoryFloorEastNight :: CardDef
factoryFloorEastNight =
  location
    "13045"
    ("Factory Floor" <:> "East")
    [Warehouse, Night]
    Triangle
    [Star, T, Moon]
    NewHorizons

managersOfficeNight :: CardDef
managersOfficeNight =
  victory 1
    $ location "13046" "Manager's Office" [Warehouse, Night] Square [T, Star] NewHorizons

loadingDockNight :: CardDef
loadingDockNight =
  location "13047" "Loading Dock" [Warehouse, Night] T [Square, Star, Triangle, Moon] NewHorizons

storageNight :: CardDef
storageNight =
  victory 1
    $ location "13048" "Storage" [Warehouse, Night] Moon [T, Triangle, Hourglass] NewHorizons

cavernEntranceShallowTunnels :: CardDef
cavernEntranceShallowTunnels =
  location
    "13049"
    ("Cavern Entrance" <:> "Shallow Tunnels")
    [Cave, Central]
    Hourglass
    [Circle, T, Plus]
    NewHorizons

cavernEntranceDarkestDepths :: CardDef
cavernEntranceDarkestDepths =
  location
    "13050"
    ("Cavern Entrance" <:> "Darkest Depths")
    [Cave, Central]
    Hourglass
    [Circle, T, Plus]
    NewHorizons

descendingTunnel :: CardDef
descendingTunnel =
  location "13051" "Descending Tunnel" [Cave, Central] Diamond [Plus] NewHorizons

hiddenLaboratoryShallowTunnels :: CardDef
hiddenLaboratoryShallowTunnels =
  victory 2
    $ locationWithUnrevealed
      "13052"
      ("Side Chamber" <:> "Shallow Tunnels")
      [Cave]
      Circle
      [Hourglass]
      ("Hidden Laboratory" <:> "Shallow Tunnels")
      [Cave, Lab]
      Circle
      [Hourglass]
      NewHorizons

lockedChamberShallowTunnels :: CardDef
lockedChamberShallowTunnels =
  victory 2
    $ locationWithUnrevealed
      "13053"
      ("Side Chamber" <:> "Shallow Tunnels")
      [Cave]
      Circle
      [Hourglass]
      ("Locked Chamber" <:> "Shallow Tunnels")
      [Cave]
      Circle
      [Hourglass]
      NewHorizons

secretChamberShallowTunnels :: CardDef
secretChamberShallowTunnels =
  locationWithUnrevealed
    "13054"
    ("Side Chamber" <:> "Shallow Tunnels")
    [Cave]
    Circle
    [Hourglass]
    ("Secret Chamber" <:> "Shallow Tunnels")
    [Cave]
    Plus
    [Hourglass, Diamond]
    NewHorizons

hiddenLaboratoryDarkestDepths :: CardDef
hiddenLaboratoryDarkestDepths =
  victory 2
    $ locationWithUnrevealed
      "13055"
      ("Side Chamber" <:> "Darkest Depths")
      [Cave]
      Circle
      [Hourglass]
      ("Hidden Laboratory" <:> "Darkest Depths")
      [Cave, Lab]
      Circle
      [Hourglass]
      NewHorizons

lockedChamberDarkestDepths :: CardDef
lockedChamberDarkestDepths =
  victory 2
    $ locationWithUnrevealed
      "13056"
      ("Side Chamber" <:> "Darkest Depths")
      [Cave]
      Circle
      [Hourglass]
      ("Locked Chamber" <:> "Darkest Depths")
      [Cave]
      Circle
      [Hourglass]
      NewHorizons

secretChamberDarkestDepths :: CardDef
secretChamberDarkestDepths =
  locationWithUnrevealed
    "13057"
    ("Side Chamber" <:> "Darkest Depths")
    [Cave]
    Circle
    [Hourglass]
    ("Secret Chamber" <:> "Darkest Depths")
    [Cave]
    Plus
    [Hourglass, Diamond]
    NewHorizons
