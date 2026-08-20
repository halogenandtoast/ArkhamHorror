module Arkham.Treachery.CardDefs.TheDunwichLegacy.TheMiskatonicMuseum where

import Arkham.Treachery.CardDefs.Import

ephemeralExhibits :: CardDef
ephemeralExhibits =
  (treachery "02145" "Ephemeral Exhibits" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Terror
    }

passageIntoTheVeil :: CardDef
passageIntoTheVeil =
  (treachery "02144" "Passage into the Veil" TheMiskatonicMuseum 3)
    { cdCardTraits = singleton Power
    }

shadowSpawned :: CardDef
shadowSpawned =
  (treachery "02142" "Shadow-spawned" TheMiskatonicMuseum 1)
    { cdCardTraits = singleton Power
    }

slitheringBehindYou :: CardDef
slitheringBehindYou =
  treachery "02146" "Slithering Behind You" TheMiskatonicMuseum 2

stalkedInTheDark :: CardDef
stalkedInTheDark =
  (treachery "02143" "Stalked in the Dark" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Tactic
    }
