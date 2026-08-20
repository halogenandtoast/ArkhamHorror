module Arkham.Treachery.CardDefs.NightOfTheZealot.AgentsOfHastur where

import Arkham.Treachery.CardDefs.Import

theYellowSign :: CardDef
theYellowSign =
  (treachery "01176" "The Yellow Sign" AgentsOfHastur 2)
    { cdCardTraits = setFromList [Omen]
    }
