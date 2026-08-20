module Arkham.Treachery.CardDefs.TheDunwichLegacy.TheEssexCountyExpress where

import Arkham.Treachery.CardDefs.Import

acrossSpaceAndTime :: CardDef
acrossSpaceAndTime =
  (weakness "02178" "Across Space and Time")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just TheEssexCountyExpress
    , cdEncounterSetQuantity = Just 4
    }

brokenRails :: CardDef
brokenRails =
  (treachery "02181" "Broken Rails" TheEssexCountyExpress 3)
    { cdCardTraits = singleton Hazard
    }

clawsOfSteam :: CardDef
clawsOfSteam =
  (treachery "02180" "Claws of Steam" TheEssexCountyExpress 3)
    { cdCardTraits = singleton Power
    }
