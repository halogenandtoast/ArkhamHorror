module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair where

import Arkham.Treachery.CardDefs.Import

blindsense :: CardDef
blindsense =
  (treachery "07054" "Blindsense" ThePitOfDespair 2)
    { cdCardTraits = singleton Scheme
    }

fromTheDepths :: CardDef
fromTheDepths =
  (treachery "07055" "From the Depths" ThePitOfDespair 3)
    { cdCardTraits = singleton Scheme
    }
