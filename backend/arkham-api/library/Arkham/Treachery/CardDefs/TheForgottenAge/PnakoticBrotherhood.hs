module Arkham.Treachery.CardDefs.TheForgottenAge.PnakoticBrotherhood where

import Arkham.Treachery.CardDefs.Import

shadowed :: CardDef
shadowed =
  (treachery "04096" "Shadowed" PnakoticBrotherhood 2)
    { cdCardTraits = singleton Scheme
    }

wordsOfPower :: CardDef
wordsOfPower =
  (treachery "04097" "Words of Power" PnakoticBrotherhood 2)
    { cdCardTraits = singleton Hex
    }
