module Arkham.Treachery.CardDefs.TheDreamEaters.MergingRealities where

import Arkham.Treachery.CardDefs.Import

glimpseOfTheUnderworld :: CardDef
glimpseOfTheUnderworld =
  (treachery "06099" "Glimpse of the Underworld" MergingRealities 2)
    { cdCardTraits = singleton Terror
    }

nightTerrors :: CardDef
nightTerrors =
  (treachery "06098" "Night Terrors" MergingRealities 2)
    { cdCardTraits = singleton Terror
    }

threadsOfReality :: CardDef
threadsOfReality =
  (treachery "06100" "Threads of Reality" MergingRealities 2)
    { cdCardTraits = singleton Power
    }
