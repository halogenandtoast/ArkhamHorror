module Arkham.Treachery.CardDefs.NightOfTheZealot where

import Arkham.Treachery.CardDefs.Import

coverUp :: CardDef
coverUp =
  signature "01001"
    $ (weakness "01007" "Cover Up")
      { cdCardTraits = setFromList [Task]
      , cdAlternateCardCodes = ["01507"]
      }

hospitalDebts :: CardDef
hospitalDebts =
  signature "01003"
    $ (weakness "01011" "Hospital Debts")
      { cdCardTraits = setFromList [Task]
      , cdAlternateCardCodes = ["01511"]
      }

abandonedAndAlone :: CardDef
abandonedAndAlone =
  signature "01005"
    $ (weakness "01015" "Abandoned and Alone")
      { cdCardTraits = setFromList [Madness]
      , cdAlternateCardCodes = ["01515"]
      }

amnesia :: CardDef
amnesia =
  (basicWeakness "01096" "Amnesia")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01596", "12097"]
    }

paranoia :: CardDef
paranoia =
  (basicWeakness "01097" "Paranoia")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01597", "12101"]
    }

haunted :: CardDef
haunted =
  (basicWeakness "01098" "Haunted")
    { cdCardTraits = setFromList [Curse]
    , cdAlternateCardCodes = ["01598"]
    }

psychosis :: CardDef
psychosis =
  (basicWeakness "01099" "Psychosis")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01599"]
    }

hypochondria :: CardDef
hypochondria =
  (basicWeakness "01100" "Hypochondria")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01600"]
    }
