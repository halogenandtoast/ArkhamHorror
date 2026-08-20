module Arkham.Treachery.CardDefs.TheCircleUndone.TheWagesOfSin where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

baneOfTheLiving :: CardDef
baneOfTheLiving =
  (treachery "05185" "Bane of the Living" TheWagesOfSin 2)
    { cdCardTraits = setFromList [Curse, Spectral]
    , cdKeywords = singleton Keyword.Peril
    }

burdensOfThePast :: CardDef
burdensOfThePast =
  (treachery "05182" "Burdens of the Past" TheWagesOfSin 2)
    { cdCardTraits = setFromList [Curse, Spectral]
    }

graveLight :: CardDef
graveLight =
  (treachery "05184" "Grave-Light" TheWagesOfSin 2)
    { cdCardTraits = singleton Curse
    }

graveLightSpectral :: CardDef
graveLightSpectral =
  (treachery "x05184" "Grave-Light" TheWagesOfSin 0)
    { cdCardTraits = singleton Curse
    , cdArt = "05184"
    }

ominousPortents :: CardDef
ominousPortents =
  (treachery "05183" "Ominous Portents" TheWagesOfSin 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = singleton Keyword.Peril
    }

punishment :: CardDef
punishment =
  (treachery "05181" "Punishment" TheWagesOfSin 2)
    { cdCardTraits = singleton Hex
    }
