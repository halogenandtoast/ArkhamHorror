module Arkham.Treachery.CardDefs.TheScarletKeys.SanguineShadows where

import Arkham.Treachery.CardDefs.Import

callingCard :: CardDef
callingCard =
  (treachery "09560" "Calling Card" SanguineShadows 2)
    { cdCardTraits = setFromList [Evidence]
    }

catAndMouse :: CardDef
catAndMouse =
  peril
    $ surge
    $ (treachery "09559" "Cat and Mouse" SanguineShadows 2)
      { cdCardTraits = setFromList [Scheme]
      }

outsmarted :: CardDef
outsmarted =
  (treachery "09561" "Outsmarted" SanguineShadows 2)
    { cdCardTraits = setFromList [Scheme]
    }
