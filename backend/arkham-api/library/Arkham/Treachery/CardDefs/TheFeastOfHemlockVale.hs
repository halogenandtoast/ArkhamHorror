module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale where

import Arkham.Treachery.CardDefs.Import

hastyRepairs :: CardDef
hastyRepairs =
  (weakness "10003" "Hasty Repairs")
    { cdCardTraits = setFromList [Blunder]
    }

failedExperiment :: CardDef
failedExperiment =
  (weakness "10008" "Failed Experiment")
    { cdCardTraits = setFromList [Blunder]
    }

wheresPa :: CardDef
wheresPa =
  (weakness "10018" "\"Where's Pa?\"")
    { cdCardTraits = setFromList [Flaw]
    }
