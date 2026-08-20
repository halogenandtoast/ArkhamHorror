module Arkham.Story.CardDefs.TheFeastOfHemlockVale.TheSecondDay where

import Arkham.Story.CardDefs.Import

dayTwo :: CardDef
dayTwo = story "10677" "Day Two" TheSecondDay & otherSideIs "10677b"

nightTwo :: CardDef
nightTwo = story "10677b" "Night Two" TheSecondDay & otherSideIs "10677"
