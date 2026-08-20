module Arkham.Treachery.CardDefs.TheDrownedCity.Domination where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

cthulhuFhtagn :: CardDef
cthulhuFhtagn =
  (treachery "11741" "\"Cthulhu fhtagn!\"" Domination 2) {cdCardTraits = setFromList [Power]}

domination :: CardDef
domination =
  (treachery "11743" "Domination" Domination 1)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

oppressiveInfluence :: CardDef
oppressiveInfluence =
  (treachery "11742" "Oppressive Influence" Domination 2) {cdCardTraits = setFromList [Power]}
