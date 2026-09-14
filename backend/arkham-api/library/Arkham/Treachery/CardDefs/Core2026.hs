{- HLINT ignore "Use camelCase" -}
module Arkham.Treachery.CardDefs.Core2026 where

import Arkham.Treachery.CardDefs.Import

inHarmsWay :: CardDef
inHarmsWay =
  signature "12001"
    $ (weakness "12003" "In Harm's Way")
      { cdCardTraits = setFromList [Flaw]
      }

breakingPoint :: CardDef
breakingPoint =
  signature "12013"
    $ (weakness "12015" "Breaking Point")
      { cdCardTraits = setFromList [Hardship]
      }

pursued :: CardDef
pursued =
  (basicWeakness "12102" "Pursued")
    { cdCardTraits = setFromList [Terror]
    }

syndicateObligations :: CardDef
syndicateObligations =
  (basicWeakness "12103" "Syndicate Obligations")
    { cdCardTraits = setFromList [Pact, Syndicate]
    }

wounded :: CardDef
wounded =
  (basicWeakness "12104" "Wounded")
    { cdCardTraits = setFromList [Injury]
    }
