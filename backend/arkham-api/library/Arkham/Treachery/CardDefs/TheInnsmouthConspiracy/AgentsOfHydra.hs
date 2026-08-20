module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.AgentsOfHydra where

import Arkham.Treachery.CardDefs.Import

psychicPull :: CardDef
psychicPull =
  (treachery "07087" "Psychic Pull" AgentsOfHydra 3)
    { cdCardTraits = singleton Power
    }
