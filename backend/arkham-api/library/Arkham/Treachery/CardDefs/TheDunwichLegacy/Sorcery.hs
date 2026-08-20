module Arkham.Treachery.CardDefs.TheDunwichLegacy.Sorcery where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

beyondTheVeil :: CardDef
beyondTheVeil =
  (treachery "02084" "Beyond the Veil" Sorcery 3)
    { cdCardTraits = setFromList [Hex]
    , cdKeywords = setFromList [Keyword.Surge]
    }

visionsOfFuturesPast :: CardDef
visionsOfFuturesPast =
  (treachery "02083" "Visions of Futures Past" Sorcery 3)
    { cdCardTraits = setFromList [Hex]
    }
