module Arkham.Treachery.CardDefs.ThePathToCarcosa.TheUnspeakableOath where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

giftOfMadnessMisery :: CardDef
giftOfMadnessMisery =
  (treachery "03187" ("Gift of Madness" <:> "Misery") TheUnspeakableOath 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

giftOfMadnessPity :: CardDef
giftOfMadnessPity =
  (treachery "03186" ("Gift of Madness" <:> "Pity") TheUnspeakableOath 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdOutOfPlayEffects = [InHandEffect]
    }

straitjacket :: CardDef
straitjacket =
  (treachery "03185" "Straitjacket" TheUnspeakableOath 2)
    { cdCardTraits = setFromList [Item, Clothing]
    }

wallsClosingIn :: CardDef
wallsClosingIn =
  (treachery "03188" "Walls Closing In" TheUnspeakableOath 3)
    { cdCardTraits = singleton Terror
    }
