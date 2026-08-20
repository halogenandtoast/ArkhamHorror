module Arkham.Treachery.CardDefs.TheCircleUndone.InexorableFate where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

fateOfAllFools :: CardDef
fateOfAllFools =
  (treachery "05108" "Fate of All Fools" InexorableFate 3)
    { cdCardTraits = setFromList [Omen, Spectral]
    , cdKeywords = singleton Keyword.Peril
    }

terrorInTheNight :: CardDef
terrorInTheNight =
  (treachery "05107" "Terror in the Night" InexorableFate 3)
    { cdCardTraits = setFromList [Terror, Spectral]
    }
