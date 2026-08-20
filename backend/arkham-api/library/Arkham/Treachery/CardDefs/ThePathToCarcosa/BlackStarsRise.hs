module Arkham.Treachery.CardDefs.ThePathToCarcosa.BlackStarsRise where

import Arkham.Treachery.CardDefs.Import

crashingFloods :: CardDef
crashingFloods =
  (treachery "03302" "Crashing Floods" BlackStarsRise 3)
    { cdCardTraits = singleton Omen
    }

worldsMerge :: CardDef
worldsMerge =
  (treachery "03303" "Worlds Merge" BlackStarsRise 3)
    { cdCardTraits = singleton Omen
    }
