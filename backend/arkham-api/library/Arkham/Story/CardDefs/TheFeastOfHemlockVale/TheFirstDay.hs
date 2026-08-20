module Arkham.Story.CardDefs.TheFeastOfHemlockVale.TheFirstDay where

import Arkham.Story.CardDefs.Import

dayOne :: CardDef
dayOne = story "10675" "Day One" TheFirstDay & otherSideIs "10675b"

nightOne :: CardDef
nightOne = story "10675b" "Night One" TheFirstDay & otherSideIs "10675"
