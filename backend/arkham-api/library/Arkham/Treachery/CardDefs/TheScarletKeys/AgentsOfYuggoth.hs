module Arkham.Treachery.CardDefs.TheScarletKeys.AgentsOfYuggoth where

import Arkham.Treachery.CardDefs.Import

tenebrousEclipse :: CardDef
tenebrousEclipse =
  (treachery "09740" "Tenebrous Eclipse" AgentsOfYuggoth 3)
    { cdCardTraits = setFromList [Omen]
    }
