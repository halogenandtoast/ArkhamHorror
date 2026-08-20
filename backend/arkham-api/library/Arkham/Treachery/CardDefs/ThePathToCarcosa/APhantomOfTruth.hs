module Arkham.Treachery.CardDefs.ThePathToCarcosa.APhantomOfTruth where

import Arkham.Treachery.CardDefs.Import

deadlyFate :: CardDef
deadlyFate =
  (treachery "03224" "Deadly Fate" APhantomOfTruth 3)
    { cdCardTraits = singleton Omen
    }

frozenInFear :: CardDef
frozenInFear =
  (treachery "03226" "Frozen in Fear" APhantomOfTruth 2)
    { cdCardTraits = singleton Terror
    }

lostSoul :: CardDef
lostSoul =
  (weakness "03227" "Lost Soul")
    { cdCardTraits = setFromList [Madness, Pact]
    , cdEncounterSet = Just APhantomOfTruth
    , cdEncounterSetQuantity = Just 4
    }

torturousChords :: CardDef
torturousChords =
  (treachery "03225" "Torturous Chords" APhantomOfTruth 3)
    { cdCardTraits = setFromList [Hex, Terror]
    }

twinSuns :: CardDef
twinSuns =
  (treachery "03223" "Twin Suns" APhantomOfTruth 2)
    { cdCardTraits = singleton Omen
    }
