module Arkham.Treachery.CardDefs.ReturnToThePathToCarcosa.ReturnToAPhantomOfTruth where

import Arkham.Treachery.CardDefs.Import

figureInTheShadows :: CardDef
figureInTheShadows =
  (treachery "52047" "Figure in the Shadows" ReturnToAPhantomOfTruth 2)
    { cdCardTraits = setFromList [Scheme]
    }
