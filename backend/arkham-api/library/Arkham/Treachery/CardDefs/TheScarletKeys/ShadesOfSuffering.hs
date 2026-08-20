module Arkham.Treachery.CardDefs.TheScarletKeys.ShadesOfSuffering where

import Arkham.Treachery.CardDefs.Import

excruciate :: CardDef
excruciate =
  (treachery "09677" "Excruciate" ShadesOfSuffering 2)
    { cdCardTraits = setFromList [Hex]
    }

spiritHarvest :: CardDef
spiritHarvest =
  peril
    $ (treachery "09678" "Spirit Harvest" ShadesOfSuffering 2)
      { cdCardTraits = setFromList [Hex]
      }
