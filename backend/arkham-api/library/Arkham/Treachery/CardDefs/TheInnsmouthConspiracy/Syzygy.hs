module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.Syzygy where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

syzygy :: CardDef
syzygy =
  (treachery "07101" "Syzygy" Syzygy 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = singleton Keyword.Peril
    }

tidalAlignment :: CardDef
tidalAlignment =
  (treachery "07100" "Tidal Alignment" Syzygy 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = singleton Keyword.Peril
    }
