module Arkham.Treachery.CardDefs.TheCircleUndone.UnionAndDisillusion where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

deathApproaches :: CardDef
deathApproaches =
  (treachery "05270" "Death Approaches" UnionAndDisillusion 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

eagerForDeath :: CardDef
eagerForDeath =
  (treachery "05268" "Eager for Death" UnionAndDisillusion 2)
    { cdCardTraits = setFromList [Omen]
    }

markedForDeath :: CardDef
markedForDeath =
  (treachery "05271" "Marked for Death" UnionAndDisillusion 2)
    { cdCardTraits = singleton Curse
    }

psychopompsSong :: CardDef
psychopompsSong =
  (treachery "05269" "Psychopomp's Song" UnionAndDisillusion 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

watchersGaze :: CardDef
watchersGaze =
  (treachery "05272" "Watcher's Gaze" UnionAndDisillusion 1)
    { cdCardTraits = singleton Curse
    }
