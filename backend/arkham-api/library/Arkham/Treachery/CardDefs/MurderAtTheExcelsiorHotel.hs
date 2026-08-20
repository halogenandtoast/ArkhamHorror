module Arkham.Treachery.CardDefs.MurderAtTheExcelsiorHotel where

import Arkham.Treachery.CardDefs.Import

bloodOnYourHands :: CardDef
bloodOnYourHands =
  (treachery "84025" "Blood on Your Hands" MurderAtTheExcelsiorHotel 4)
    { cdCardTraits = singleton Terror
    }

drivenToMadness :: CardDef
drivenToMadness =
  (treachery "84024" "Driven to Madness" MurderAtTheExcelsiorHotel 3)
    { cdCardTraits = singleton Curse
    }

incriminatingEvidence :: CardDef
incriminatingEvidence =
  (treachery "84026" "Incriminating Evidence" MurderAtTheExcelsiorHotel 2)
    { cdCardTraits = singleton Evidence
    }

noxiousFumes :: CardDef
noxiousFumes =
  (treachery "84023" "Noxious Fumes" MurderAtTheExcelsiorHotel 2)
    { cdCardTraits = singleton Hazard
    }

violentOutburst :: CardDef
violentOutburst =
  (treachery "84027" "Violent Outburst" MurderAtTheExcelsiorHotel 3)
    { cdCardTraits = singleton Curse
    }

whatHaveYouDone :: CardDef
whatHaveYouDone =
  (weakness "84007" "What Have You Done?")
    { cdCardTraits = singleton Madness
    , cdEncounterSet = Just MurderAtTheExcelsiorHotel
    , cdEncounterSetQuantity = Just 1
    }
