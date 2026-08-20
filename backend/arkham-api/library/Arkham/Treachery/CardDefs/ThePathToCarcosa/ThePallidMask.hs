module Arkham.Treachery.CardDefs.ThePathToCarcosa.ThePallidMask where

import Arkham.Treachery.CardDefs.Import

eyesInTheWalls :: CardDef
eyesInTheWalls =
  (treachery "03260" "Eyes in the Walls" ThePallidMask 3)
    { cdCardTraits = singleton Terror
    }

thePitBelow :: CardDef
thePitBelow =
  (treachery "03262" "The Pit Below" ThePallidMask 3)
    { cdCardTraits = singleton Hazard
    }

theShadowBehindYou :: CardDef
theShadowBehindYou =
  (treachery "03261" "The Shadow Behind You" ThePallidMask 3)
    { cdCardTraits = singleton Terror
    }
