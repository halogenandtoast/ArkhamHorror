module Arkham.Act.CardDefs.TheForgottenAge.TurnBackTime where

import Arkham.Act.CardDefs.Import

intoTheRuinsOnceAgain :: CardDef
intoTheRuinsOnceAgain = (act "04345" "Into the Ruins Once Again" 1 TurnBackTime) {cdVengeancePoints = Just 2}

theChamberOfStillRemains :: CardDef
theChamberOfStillRemains = (act "04346" "The Chamber of Still Remains" 2 TurnBackTime) {cdVengeancePoints = Just 2}

momentOfDoom :: CardDef
momentOfDoom = act "04347" "Moment of Doom" 3 TurnBackTime
