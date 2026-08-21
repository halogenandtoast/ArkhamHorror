module Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood where

import Arkham.Location.CardDefs.Import

waterStreetDawn :: CardDef
waterStreetDawn =
  location "13008" "Water Street" [Arkham, Dawn] Triangle [Circle, Square, Heart] RiverOfBlood

waterStreetDusk :: CardDef
waterStreetDusk =
  location "13009" "Water Street" [Arkham, Dusk] Triangle [Circle, Square, Heart] RiverOfBlood

garrisonStreetDawn :: CardDef
garrisonStreetDawn =
  location "13010" "Garrison Street" [Arkham, Dawn] Square [Circle, Triangle, Heart] RiverOfBlood

garrisonStreetDusk :: CardDef
garrisonStreetDusk =
  location "13011" "Garrison Street" [Arkham, Dusk] Square [Circle, Triangle, Heart] RiverOfBlood

riverDocksDawn :: CardDef
riverDocksDawn =
  location "13012" "River Docks" [Arkham, Dawn] T [Circle, Moon, Hourglass] RiverOfBlood

riverDocksDusk :: CardDef
riverDocksDusk =
  location "13013" "River Docks" [Arkham, Dusk] T [Circle, Moon, Hourglass] RiverOfBlood

mainStreetDawn :: CardDef
mainStreetDawn =
  location "13014" "Main Street" [Arkham, Dawn] Moon [Circle, T, Hourglass] RiverOfBlood

mainStreetDusk :: CardDef
mainStreetDusk =
  location "13015" "Main Street" [Arkham, Dusk] Moon [Circle, T, Hourglass] RiverOfBlood

erwinBridgeDawn :: CardDef
erwinBridgeDawn =
  location
    "13016"
    "Erwin Bridge"
    [Arkham, Central, Dawn]
    Circle
    [Triangle, Square, T, Moon]
    RiverOfBlood

erwinBridgeDusk :: CardDef
erwinBridgeDusk =
  location
    "13017"
    "Erwin Bridge"
    [Arkham, Central, Dusk]
    Circle
    [Triangle, Square, T, Moon]
    RiverOfBlood

unvisitedIsleDawn :: CardDef
unvisitedIsleDawn =
  victory 2
    $ locationWithUnrevealed
      "13018"
      "Unvisited Isle"
      [Lair, Dawn]
      NoSymbol
      []
      "Unvisited Isle"
      [Lair, Dawn]
      NoSymbol
      [T]
      RiverOfBlood

unvisitedIsleDusk :: CardDef
unvisitedIsleDusk =
  victory 2
    $ locationWithUnrevealed
      "13019"
      "Unvisited Isle"
      [Lair, Dusk]
      NoSymbol
      []
      "Unvisited Isle"
      [Lair, Dawn]
      NoSymbol
      [T]
      RiverOfBlood

waterfrontWarehouseDawn :: CardDef
waterfrontWarehouseDawn =
  victory 2
    $ location "13020" "Waterfront Warehouse" [Arkham, Lair, Dawn] Heart [Triangle, Square] RiverOfBlood

waterfrontWarehouseDusk :: CardDef
waterfrontWarehouseDusk =
  victory 2
    $ location "13021" "Waterfront Warehouse" [Arkham, Lair, Dusk] Heart [Triangle, Square] RiverOfBlood

backAlleyDawn :: CardDef
backAlleyDawn =
  victory 2
    $ location "13022" "Back Alley" [Arkham, Lair, Dawn] Hourglass [T, Moon] RiverOfBlood

backAlleyDusk :: CardDef
backAlleyDusk =
  victory 2
    $ location "13023" "Back Alley" [Arkham, Lair, Dusk] Hourglass [T, Moon] RiverOfBlood
