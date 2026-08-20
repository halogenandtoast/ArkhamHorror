module Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheLostSister where

import Arkham.Location.CardDefs.Import

akwan :: CardDef
akwan = location_ "10575" "Akwan" [Coastal] TheLostSister

fungalCave :: CardDef
fungalCave =
  victory 1
    $ locationWithUnrevealed_ "10582" "Cavern" [Cave, Dark] "Fungal Cave" [Cave, Lair, Dark] TheLostSister

hiddenCoveTheLostSister :: CardDef
hiddenCoveTheLostSister =
  locationWithUnrevealed_ "10577" "Cavern" [Cave, Dark] "Hidden Cove" [Coastal] TheLostSister

openCave :: CardDef
openCave =
  quantity 2
    $ locationWithUnrevealed_ "10581" "Cavern" [Cave, Dark] "Open Cave" [Cave, Dark] TheLostSister

rockyShoreline :: CardDef
rockyShoreline =
  locationWithUnrevealed_ "10579" "Cavern" [Cave, Dark] "Rocky Shoreline" [Coastal] TheLostSister

suspendedGraveyard :: CardDef
suspendedGraveyard =
  quantity 2
    $ locationWithUnrevealed_
      "10576"
      "Cavern"
      [Cave, Dark]
      "Suspended Graveyard"
      [Cave, Coastal]
      TheLostSister

undergroundPools :: CardDef
undergroundPools =
  locationWithUnrevealed_ "10580" "Cavern" [Cave, Dark] "Underground Pools" [Cave, Dark] TheLostSister

weedChokedBeach :: CardDef
weedChokedBeach =
  locationWithUnrevealed_
    "10578"
    "Cavern"
    [Cave, Dark]
    "Weed-Choked Beach"
    [Coastal, Cave]
    TheLostSister
