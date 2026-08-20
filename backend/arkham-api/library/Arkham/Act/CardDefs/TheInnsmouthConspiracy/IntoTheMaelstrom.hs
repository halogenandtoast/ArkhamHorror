module Arkham.Act.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom where

import Arkham.Act.CardDefs.Import

backIntoTheDepths :: CardDef
backIntoTheDepths = act "07315" "Back into the Depths" 1 IntoTheMaelstrom

cityOfTheDeepV1 :: CardDef
cityOfTheDeepV1 = (act "07316" "City of the Deep (v.I)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

cityOfTheDeepV2 :: CardDef
cityOfTheDeepV2 = (act "07317" "City of the Deep (v.II)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

cityOfTheDeepV3 :: CardDef
cityOfTheDeepV3 = (act "07318" "City of the Deep (v.III)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}
