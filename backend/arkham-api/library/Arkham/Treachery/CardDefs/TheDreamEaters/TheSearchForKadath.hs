module Arkham.Treachery.CardDefs.TheDreamEaters.TheSearchForKadath where

import Arkham.Treachery.CardDefs.Import

dreamlandsEclipse :: CardDef
dreamlandsEclipse =
  (treachery "06096" "Dreamlands Eclipse" Dreamlands 2)
    { cdCardTraits = singleton Power
    }

prismaticPhenomenon :: CardDef
prismaticPhenomenon =
  (treachery "06097" "Prismatic Phenomenon" Dreamlands 2)
    { cdCardTraits = singleton Power
    }

songOfTheMagahBird :: CardDef
songOfTheMagahBird =
  (treachery "06153" "Song of the Magah Bird" TheSearchForKadath 2)
    { cdCardTraits = singleton Curse
    }

wondrousLands :: CardDef
wondrousLands =
  (treachery "06154" "Wondrous Lands" TheSearchForKadath 2)
    { cdCardTraits = singleton Power
    }

zoogBurrow :: CardDef
zoogBurrow =
  (treachery "06109" "Zoog Burrow" Zoogs 1)
    { cdCardTraits = singleton Hazard
    }
