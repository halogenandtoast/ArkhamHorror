module Arkham.Treachery.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

lostInTheWoods :: CardDef
lostInTheWoods =
  (treachery "06062" "Lost in the Woods" BeyondTheGatesOfSleep 2)
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Surge
    }
