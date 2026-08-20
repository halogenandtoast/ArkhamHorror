module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ALightInTheFog where

import Arkham.Treachery.CardDefs.Import

hideousLullaby :: CardDef
hideousLullaby =
  (treachery "07256" "Hideous Lullaby" ALightInTheFog 3)
    { cdCardTraits = singleton Terror
    }

kissOfBrine :: CardDef
kissOfBrine =
  (treachery "07257" "Kiss of Brine" ALightInTheFog 2)
    { cdCardTraits = setFromList [Curse, Hazard]
    }

takenCaptive :: CardDef
takenCaptive =
  (treachery "07260" "Taken Captive" ALightInTheFog 2)
    { cdCardTraits = singleton Scheme
    }

totality :: CardDef
totality =
  (treachery "07258" "Totality" ALightInTheFog 2)
    { cdCardTraits = setFromList [Omen, Terror]
    }

worthHisSalt :: CardDef
worthHisSalt =
  (treachery "07259" "Worth His Salt" ALightInTheFog 2)
    { cdCardTraits = singleton Scheme
    }
