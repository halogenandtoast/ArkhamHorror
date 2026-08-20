module Arkham.Treachery.CardDefs.ReturnToNightOfTheZealot.ReturnToTheGathering where

import Arkham.Treachery.CardDefs.Import

theZealotsSeal :: CardDef
theZealotsSeal =
  (treachery "50024" "The Zealot's Seal" ReturnToTheGathering 2)
    { cdCardTraits = setFromList [Hex]
    }
