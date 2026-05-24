module Arkham.Act.CardDefs.TheFeastOfHemlockVale where

import Arkham.Act.CardDefs.Import

descentIntoTheMines :: CardDef
descentIntoTheMines = act "10505" "Descent into the Mines" 1 WrittenInRock

theUndergroundMaze :: CardDef
theUndergroundMaze = act "10506" "The Underground Maze" 2 WrittenInRock

strangeInfestation :: CardDef
strangeInfestation = act "10529" "Strange Infestation" 1 HemlockHouse

againstTheHouse :: CardDef
againstTheHouse = act "10530" "Against the House" 2 HemlockHouse

theHeartOfTheHouse :: CardDef
theHeartOfTheHouse = act "10531" "Heart of the House" 2 HemlockHouse

aLostLegacy :: CardDef
aLostLegacy = act "10553" "A Lost Legacy" 1 TheSilentHeath

searchingTheHeath :: CardDef
searchingTheHeath = act "10554" "Searching the Heath" 2 TheSilentHeath

theMissingSibling :: CardDef
theMissingSibling = act "10572" "The Missing Sibling" 1 TheLostSister

onTheTrail :: CardDef
onTheTrail = act "10573" "On the Trail" 2 TheLostSister

faceToCarapace :: CardDef
faceToCarapace = act "10574" "Face to Carapace" 3 TheLostSister

desperateSearch :: CardDef
desperateSearch = (act "10607a" "Desperate Search" 1 TheTwistedHollow) {cdOtherSide = Just "10607b"}

wheresBertie :: CardDef
wheresBertie = act "10608" "Where's Bertie" 2 TheTwistedHollow

dawnOfTheFirstDay :: CardDef
dawnOfTheFirstDay = act "10682" "Dawn of the First Day" 1 DayOfRest

dawnOfTheSecondDay :: CardDef
dawnOfTheSecondDay = act "10684" "Dawn of the Second Day" 1 DayOfRain
