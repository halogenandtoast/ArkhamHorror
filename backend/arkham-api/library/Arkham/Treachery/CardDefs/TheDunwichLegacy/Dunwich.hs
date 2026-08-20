module Arkham.Treachery.CardDefs.TheDunwichLegacy.Dunwich where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Treachery.CardDefs.Import

sordidAndSilent :: CardDef
sordidAndSilent =
  (treachery "02089" "Sordid and Silent" EncounterSet.Dunwich 2)
    { cdCardTraits = setFromList [Terror]
    }

unhallowedCountry :: CardDef
unhallowedCountry =
  (treachery "02088" "Unhallowed Country" EncounterSet.Dunwich 2)
    { cdCardTraits = setFromList [Terror]
    }
