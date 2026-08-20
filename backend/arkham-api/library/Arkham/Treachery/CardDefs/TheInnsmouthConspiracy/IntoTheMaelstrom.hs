module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

conspiracyOfDeepOnes :: CardDef
conspiracyOfDeepOnes =
  (treachery "07336" "Conspiracy of Deep Ones" IntoTheMaelstrom 2)
    { cdCardTraits = setFromList [Scheme]
    , cdKeywords = setFromList [Keyword.Peril]
    }

thalassophobia :: CardDef
thalassophobia =
  (treachery "07337" "Thalassophobia" IntoTheMaelstrom 2)
    { cdCardTraits = setFromList [Terror]
    }

treacherousDepths :: CardDef
treacherousDepths =
  (treachery "07335" "Treacherous Depths" IntoTheMaelstrom 3)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Peril]
    }
