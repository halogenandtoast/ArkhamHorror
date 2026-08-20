module Arkham.Treachery.CardDefs.TheForgottenAge.GuardiansOfTime where

import Arkham.Treachery.CardDefs.Import

arrowsFromTheTrees :: CardDef
arrowsFromTheTrees =
  (treachery "04087" "Arrows from the Trees" GuardiansOfTime 2)
    { cdCardTraits = singleton Scheme
    }
