module Arkham.Treachery.CardDefs.TheCircleUndone.ForTheGreaterGood where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

beneathTheLodge :: CardDef
beneathTheLodge =
  (treachery "05225" "Beneath the Lodge" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    }

callToOrder :: CardDef
callToOrder =
  (treachery "05223" "Call to Order" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    }

expulsion :: CardDef
expulsion =
  (treachery "05224" "Expulsion" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    }

markOfTheOrder :: CardDef
markOfTheOrder =
  (treachery "05226" "Mark of the Order" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Surge
    }
