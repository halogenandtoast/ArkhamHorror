module Arkham.Act.CardDefs.TheInnsmouthConspiracy where

import Arkham.Act.CardDefs.Import

thePit :: CardDef
thePit = act "07045" "The Pit" 1 ThePitOfDespair

theEscape :: CardDef
theEscape = act "07046" "The Escape" 2 ThePitOfDespair

theSearchForAgentHarper :: CardDef
theSearchForAgentHarper = act "07060" "The Search for Agent Harper" 1 TheVanishingOfElinaHarper

theRescue :: CardDef
theRescue = act "07061" "The Rescue" 2 TheVanishingOfElinaHarper

throughTheLabyrinth :: CardDef
throughTheLabyrinth = act "07128" "Through the Labyrinth" 1 InTooDeep

reefOfMysteries :: CardDef
reefOfMysteries = act "07167" "Reef of Mysteries" 1 DevilReef

pedalToTheMetal :: CardDef
pedalToTheMetal = act "07202" "Pedal to the Metal" 1 HorrorInHighGear

theLighthouse :: CardDef
theLighthouse = act "07236" "The Lighthouse" 1 ALightInTheFog

findingThePath :: CardDef
findingThePath = act "07237" "Finding the Path" 2 ALightInTheFog

worshippersOfTheDeep :: CardDef
worshippersOfTheDeep = act "07238" "Worshippers of the Deep" 3 ALightInTheFog

theFirstOath :: CardDef
theFirstOath = act "07280" "The First Oath" 1 TheLairOfDagon

theSecondOath :: CardDef
theSecondOath = act "07281" "The Second Oath" 2 TheLairOfDagon

theThirdOath :: CardDef
theThirdOath = act "07282" "The Third Oath" 3 TheLairOfDagon

backIntoTheDepths :: CardDef
backIntoTheDepths = act "07315" "Back into the Depths" 1 IntoTheMaelstrom

cityOfTheDeepV1 :: CardDef
cityOfTheDeepV1 = (act "07316" "City of the Deep (v.I)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

cityOfTheDeepV2 :: CardDef
cityOfTheDeepV2 = (act "07317" "City of the Deep (v.II)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

cityOfTheDeepV3 :: CardDef
cityOfTheDeepV3 = (act "07318" "City of the Deep (v.III)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}
