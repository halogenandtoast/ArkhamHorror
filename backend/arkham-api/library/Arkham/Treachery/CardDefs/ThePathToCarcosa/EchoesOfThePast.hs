module Arkham.Treachery.CardDefs.ThePathToCarcosa.EchoesOfThePast where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

ledAstray :: CardDef
ledAstray =
  (treachery "03145" "Led Astray" EchoesOfThePast 3)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Peril
    }

theCultsSearch :: CardDef
theCultsSearch =
  (treachery "03146" "The Cult's Search" EchoesOfThePast 2)
    { cdCardTraits = singleton Scheme
    }
