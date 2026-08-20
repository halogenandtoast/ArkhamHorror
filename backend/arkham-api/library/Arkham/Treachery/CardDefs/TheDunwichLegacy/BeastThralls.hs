module Arkham.Treachery.CardDefs.TheDunwichLegacy.BeastThralls where

import Arkham.Treachery.CardDefs.Import

alteredBeast :: CardDef
alteredBeast =
  (treachery "02096" "Altered Beast" BeastThralls 2)
    { cdCardTraits = setFromList [Power]
    }
