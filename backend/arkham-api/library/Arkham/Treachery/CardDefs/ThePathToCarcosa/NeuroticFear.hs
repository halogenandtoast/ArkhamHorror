module Arkham.Treachery.CardDefs.ThePathToCarcosa.NeuroticFear where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

melancholy :: CardDef
melancholy =
  (treachery "52077" "Melancholy" NeuroticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

painfulReflection :: CardDef
painfulReflection =
  (treachery "52078" "Painful Reflection" NeuroticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

voiceOfTrunembra :: CardDef
voiceOfTrunembra =
  (treachery "52076" "Voice of Tru'nembra" NeuroticFear 3)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril]
    }
