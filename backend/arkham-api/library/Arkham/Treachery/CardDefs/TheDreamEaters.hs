module Arkham.Treachery.CardDefs.TheDreamEaters where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

rookieMistake :: CardDef
rookieMistake =
  signature "06001"
    $ (weakness "06007" "Rookie Mistake")
      { cdCardTraits = setFromList [Blunder, Flaw]
      }

shockingDiscovery :: CardDef
shockingDiscovery =
  signature "06002"
    $ (weakness "06009" "Shocking Discovery")
      { cdCardTraits = setFromList [Blunder, Mystery]
      , cdOutOfPlayEffects = [InSearchEffect]
      }

detachedFromReality :: CardDef
detachedFromReality =
  signature "06004"
    $ (weakness "06014" "Detached from Reality")
      { cdCardTraits = setFromList [Madness]
      }

bloodlust :: CardDef
bloodlust =
  (weakness "06019" "Bloodlust")
    { cdCardTraits = setFromList [Madness]
    , cdKeywords = singleton (Keyword.Bonded 3 "06018")
    }

selfCentered :: CardDef
selfCentered =
  (basicWeakness "06035" "Self-Centered")
    { cdCardTraits = setFromList [Flaw]
    , cdDeckRestrictions = [MultiplayerOnly]
    }

narcolepsy :: CardDef
narcolepsy =
  (basicWeakness "06037" "Narcolepsy")
    { cdCardTraits = setFromList [Madness]
    , cdDeckRestrictions = [MultiplayerOnly]
    }
