module Arkham.Treachery.CardDefs.EdgeOfTheEarth.NamelessHorrors where

import Arkham.Treachery.CardDefs.Import

blasphemousVisions :: CardDef
blasphemousVisions =
  (treachery "08703" "Blasphemous Visions" NamelessHorrors 2)
    { cdCardTraits = setFromList [Terror]
    }

glimpseTheUnspeakable :: CardDef
glimpseTheUnspeakable =
  peril
    $ (treachery "08704" "Glimpse the Unspeakable" NamelessHorrors 2)
      { cdCardTraits = setFromList [Terror]
      }

nightmarishVapors :: CardDef
nightmarishVapors =
  peril
    $ (treachery "08705" "Nightmarish Vapors" NamelessHorrors 2)
      { cdCardTraits = setFromList [Terror]
      }
