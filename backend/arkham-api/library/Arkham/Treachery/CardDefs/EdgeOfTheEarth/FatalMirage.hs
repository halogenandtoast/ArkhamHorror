module Arkham.Treachery.CardDefs.EdgeOfTheEarth.FatalMirage where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

anamnesis :: CardDef
anamnesis =
  (treachery "08586" "Anamnesis" FatalMirage 3)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril]
    }

evanescentMist :: CardDef
evanescentMist =
  (treachery "08585" "Evanescent Mist" FatalMirage 3)
    { cdCardTraits = setFromList [Curse, Hazard]
    }
