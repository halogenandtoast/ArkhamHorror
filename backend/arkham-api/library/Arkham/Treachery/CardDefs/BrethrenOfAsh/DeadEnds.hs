module Arkham.Treachery.CardDefs.BrethrenOfAsh.DeadEnds where

import Arkham.Treachery.CardDefs.Import

raisingSuspicions :: CardDef
raisingSuspicions =
  (treachery "12160" "Raising Suspicions" DeadEnds 2)
    { cdCardTraits = singleton Blunder
    }

redHerring :: CardDef
redHerring =
  (treachery "12161" "Red Herring" DeadEnds 2)
    { cdCardTraits = singleton Scheme
    }
