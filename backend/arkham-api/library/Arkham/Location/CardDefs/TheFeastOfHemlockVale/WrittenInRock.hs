module Arkham.Location.CardDefs.TheFeastOfHemlockVale.WrittenInRock where

import Arkham.Location.CardDefs.Import

alkalineRailA :: CardDef
alkalineRailA =
  locationWithUnrevealed_ "10512a" "Rail Tunnel" [Rail] "Alkaline Rail" [Rail] WrittenInRock
    & railIcons [East, West]

alkalineRailB :: CardDef
alkalineRailB =
  locationWithUnrevealed_ "10512b" "Rail Tunnel" [Rail] "Alkaline Rail" [Rail] WrittenInRock
    & railIcons [North, South]

controlStation :: CardDef
controlStation =
  location_ "10508" "Control Station" [Rail, Station] WrittenInRock
    & railIcons [North, East]

forkedRail :: CardDef
forkedRail =
  quantity 3
    $ locationWithUnrevealed_ "10515" "Rail Tunnel" [Rail] "Forked Rail" [Rail] WrittenInRock
    & railIcons [North, East, South, West]

leftTurnA :: CardDef
leftTurnA =
  locationWithUnrevealed_ "10510a" "Rail Tunnel" [Rail] "Left Turn" [Rail] WrittenInRock
    & railIcons [North, West]

leftTurnB :: CardDef
leftTurnB =
  locationWithUnrevealed_ "10510b" "Rail Tunnel" [Rail] "Left Turn" [Rail] WrittenInRock
    & railIcons [North, West]

railBridge :: CardDef
railBridge =
  locationWithUnrevealed_ "10516" "Rail Tunnel" [Rail] "Rail Bridge" [Rail] WrittenInRock
    & railIcons [North, South]

railExit :: CardDef
railExit =
  victory 1
    $ location_ "10509" "Rail Exit" [Rail] WrittenInRock
    & railIcons [South]

rightTurnA :: CardDef
rightTurnA =
  locationWithUnrevealed_ "10511a" "Rail Tunnel" [Rail] "Right Turn" [Rail] WrittenInRock
    & railIcons [East, South]

rightTurnB :: CardDef
rightTurnB =
  locationWithUnrevealed_ "10511b" "Rail Tunnel" [Rail] "Right Turn" [Rail] WrittenInRock
    & railIcons [East, South]

sunkenRailA :: CardDef
sunkenRailA =
  locationWithUnrevealed_ "10514a" "Rail Tunnel" [Rail] "Sunken Rail" [Rail] WrittenInRock
    & railIcons [East, South, West]

sunkenRailB :: CardDef
sunkenRailB =
  locationWithUnrevealed_ "10514b" "Rail Tunnel" [Rail] "Sunken Rail" [Rail] WrittenInRock
    & railIcons [North, East, South]

warpedRailA :: CardDef
warpedRailA =
  victory 1
    $ locationWithUnrevealed_ "10513a" "Rail Tunnel" [Rail] "Warped Rail" [Rail] WrittenInRock
    & railIcons [East, West]

warpedRailB :: CardDef
warpedRailB =
  victory 1
    $ locationWithUnrevealed_ "10513b" "Rail Tunnel" [Rail] "Warped Rail" [Rail] WrittenInRock
    & railIcons [North, East, South]
