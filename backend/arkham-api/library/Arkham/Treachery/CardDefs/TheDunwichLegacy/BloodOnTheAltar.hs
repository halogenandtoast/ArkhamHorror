module Arkham.Treachery.CardDefs.TheDunwichLegacy.BloodOnTheAltar where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

kidnapped :: CardDef
kidnapped = treachery "02220" "Kidnapped!" BloodOnTheAltar 3

psychopompsSong :: CardDef
psychopompsSong =
  (treachery "02221" "Psychopomp's Song" BloodOnTheAltar 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

rottingRemains :: CardDef
rottingRemains =
  (treachery "02223" "Rotting Remains" BloodOnTheAltar 3)
    { cdCardTraits = singleton Terror
    }

strangeSigns :: CardDef
strangeSigns =
  (treachery "02222" "Strange Signs" BloodOnTheAltar 2)
    { cdCardTraits = singleton Omen
    }
