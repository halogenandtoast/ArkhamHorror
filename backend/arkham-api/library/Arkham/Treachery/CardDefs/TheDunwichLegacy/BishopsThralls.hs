module Arkham.Treachery.CardDefs.TheDunwichLegacy.BishopsThralls where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

lightOfAforgomon :: CardDef
lightOfAforgomon =
  (treachery "02085" "Light of Aforgomon" BishopsThralls 2)
    { cdCardTraits = setFromList [Pact, Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }
