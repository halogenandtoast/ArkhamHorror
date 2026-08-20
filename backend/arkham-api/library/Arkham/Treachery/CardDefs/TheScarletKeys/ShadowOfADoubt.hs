module Arkham.Treachery.CardDefs.TheScarletKeys.ShadowOfADoubt where

import Arkham.Treachery.CardDefs.Import

knivesInTheDark :: CardDef
knivesInTheDark =
  (treachery "09722" "Knives in the Dark" ShadowOfADoubt 2)
    { cdCardTraits = setFromList [Scheme]
    }

undercover :: CardDef
undercover =
  (treachery "09723" "Undercover" ShadowOfADoubt 2)
    { cdCardTraits = setFromList [Scheme]
    }
