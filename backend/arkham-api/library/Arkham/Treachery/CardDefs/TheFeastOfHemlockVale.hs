module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale where

import Arkham.Treachery.CardDefs.Import

hastyRepairs :: CardDef
hastyRepairs =
  signature "10001"
    $ (weakness "10003" "Hasty Repairs")
      { cdCardTraits = setFromList [Blunder]
      }

failedExperiment :: CardDef
failedExperiment =
  signature "10004"
    $ (weakness "10008" "Failed Experiment")
      { cdCardTraits = setFromList [Blunder]
      }

wheresPa :: CardDef
wheresPa =
  signature "10015"
    $ (weakness "10018" "\"Where's Pa?\"")
      { cdCardTraits = setFromList [Flaw]
      }
