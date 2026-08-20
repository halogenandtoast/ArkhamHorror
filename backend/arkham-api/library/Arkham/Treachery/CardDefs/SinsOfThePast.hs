module Arkham.Treachery.CardDefs.SinsOfThePast where

import Arkham.Treachery.CardDefs.Import

chillingPresence :: CardDef
chillingPresence =
  (treachery "84042" "Chilling Presence" SinsOfThePast 3)
    { cdCardTraits = singleton Terror
    }
