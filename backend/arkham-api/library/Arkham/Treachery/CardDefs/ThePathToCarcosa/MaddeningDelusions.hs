module Arkham.Treachery.CardDefs.ThePathToCarcosa.MaddeningDelusions where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

maddeningDelusions :: CardDef
maddeningDelusions =
  surge
    (treachery "52075" "Maddening Delusions" MaddeningDelusions 2)
      { cdCardTraits = setFromList [Terror]
      }

visionsInYourMindDeath :: CardDef
visionsInYourMindDeath =
  (treachery "52073" ("Visions in Your Mind" <:> "Death") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

visionsInYourMindFailure :: CardDef
visionsInYourMindFailure =
  (treachery "52072" ("Visions in Your Mind" <:> "Failure") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

visionsInYourMindHatred :: CardDef
visionsInYourMindHatred =
  (treachery "52074" ("Visions in Your Mind" <:> "Hatred") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

visionsInYourMindHorrors :: CardDef
visionsInYourMindHorrors =
  (treachery "52071" ("Visions in Your Mind" <:> "Horrors") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }
