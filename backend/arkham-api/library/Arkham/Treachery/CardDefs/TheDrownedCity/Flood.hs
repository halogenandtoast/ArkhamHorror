module Arkham.Treachery.CardDefs.TheDrownedCity.Flood where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

deadlyTorrent :: CardDef
deadlyTorrent =
  (treachery "11736" "Deadly Torrent" Flood 2) {cdCardTraits = setFromList [Hazard]}

ominousSilence :: CardDef
ominousSilence =
  (treachery "11738" "Ominous Silence" Flood 2) {cdCardTraits = setFromList [Terror]}

somethingInTheWater :: CardDef
somethingInTheWater =
  (treachery "11737" "Something in the Water" Flood 2)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Surge]
    }
