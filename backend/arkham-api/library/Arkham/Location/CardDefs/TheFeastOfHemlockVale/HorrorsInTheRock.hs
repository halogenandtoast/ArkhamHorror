module Arkham.Location.CardDefs.TheFeastOfHemlockVale.HorrorsInTheRock where

import Arkham.Location.CardDefs.Import

alkalineForest :: CardDef
alkalineForest =
  locationWithUnrevealed_
    "10717"
    "Cavern"
    [Cave, Dark]
    "Alkaline Forest"
    [Cave, Dark]
    HorrorsInTheRock

dryBurrow :: CardDef
dryBurrow =
  quantity 2
    $ locationWithUnrevealed_ "10716" "Cavern" [Cave, Dark] "Dry Burrow" [Cave, Dark] HorrorsInTheRock

iridescentPassage :: CardDef
iridescentPassage =
  locationWithUnrevealed_
    "10718"
    "Cavern"
    [Cave, Dark]
    "Iridescent Passage"
    [Cave]
    HorrorsInTheRock

mineralTunnel :: CardDef
mineralTunnel =
  locationWithUnrevealed_ "10720" "Cavern" [Cave, Dark] "Mineral Tunnel" [Cave, Dark] HorrorsInTheRock

overgrownTunnel :: CardDef
overgrownTunnel =
  locationWithUnrevealed_
    "10719"
    "Cavern"
    [Cave, Dark]
    "Overgrown Tunnel"
    [Cave, Dark]
    HorrorsInTheRock
