module Arkham.Treachery.CardDefs.TheForgottenAge.TheCityOfArchives where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Treachery.CardDefs.Import

captiveMind :: CardDef
captiveMind =
  (treachery "04263" "Captive Mind" EncounterSet.TheCityOfArchives 2)
    { cdCardTraits = singleton Hex
    }

cruelInterrogations :: CardDef
cruelInterrogations =
  (treachery "04261" "Cruel Interrogations" EncounterSet.TheCityOfArchives 3)
    { cdCardTraits = setFromList [Injury, Terror]
    }

lostHumanity :: CardDef
lostHumanity =
  (treachery "04262" "Lost Humanity" EncounterSet.TheCityOfArchives 2)
    { cdCardTraits = singleton Terror
    }

outOfBodyExperience :: CardDef
outOfBodyExperience =
  (weakness "04264" "Out of Body Experience")
    { cdCardTraits = setFromList [Madness, Paradox]
    , cdEncounterSet = Just TheCityOfArchives
    , cdEncounterSetQuantity = Just 4
    }

yithianPresence :: CardDef
yithianPresence =
  (treachery "04260" "Yithian Presence" EncounterSet.TheCityOfArchives 3)
    { cdCardTraits = setFromList [Power, Terror]
    }
