module Arkham.Treachery.CardDefs.TheDreamEaters.DarkSideOfTheMoon where

import Arkham.Treachery.CardDefs.Import

closeWatch :: CardDef
closeWatch =
  (treachery "06230" "Close Watch" DarkSideOfTheMoon 3)
    { cdCardTraits = singleton Scheme
    }

falseAwakening :: CardDef
falseAwakening =
  (weakness "06233" "False Awakening")
    { cdCardTraits = setFromList [Curse]
    , cdEncounterSet = Just DarkSideOfTheMoon
    , cdEncounterSetQuantity = Just 1
    }

forcedIntoHiding :: CardDef
forcedIntoHiding =
  (treachery "06231" "Forced into Hiding" DarkSideOfTheMoon 3)
    { cdCardTraits = singleton Terror
    }

lunarPatrol :: CardDef
lunarPatrol =
  (treachery "06232" "Lunar Patrol" DarkSideOfTheMoon 2)
    { cdCardTraits = singleton Scheme
    }
