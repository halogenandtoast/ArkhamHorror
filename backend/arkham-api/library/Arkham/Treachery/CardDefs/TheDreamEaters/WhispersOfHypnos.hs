module Arkham.Treachery.CardDefs.TheDreamEaters.WhispersOfHypnos where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

whispersOfHypnos :: CardDef
whispersOfHypnos =
  (treachery "06090" "Whispers of Hypnos" WhispersOfHypnos 3)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }
