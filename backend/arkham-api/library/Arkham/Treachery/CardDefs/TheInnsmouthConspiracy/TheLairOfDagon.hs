module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon where

import Arkham.Treachery.CardDefs.Import

esotericRitual :: CardDef
esotericRitual =
  (treachery "07297" "Esoteric Ritual" TheLairOfDagon 3)
    { cdCardTraits = setFromList [Hex]
    }

fulfillTheOaths :: CardDef
fulfillTheOaths =
  (treachery "07295" "Fulfill the Oaths" TheLairOfDagon 3)
    { cdCardTraits = setFromList [Hazard]
    }

heraldsOfTheDeep :: CardDef
heraldsOfTheDeep =
  (treachery "07298" "Heralds of the Deep" TheLairOfDagon 3)
    { cdCardTraits = setFromList [Curse]
    }

secretGathering :: CardDef
secretGathering =
  (treachery "07296" "Secret Gathering" TheLairOfDagon 2)
    { cdCardTraits = setFromList [Hex]
    }

stoneBarrier :: CardDef
stoneBarrier =
  (treachery "07299" "Stone Barrier" TheLairOfDagon 2)
    { cdCardTraits = setFromList [Obstacle]
    }
