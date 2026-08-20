module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.DevilReef where

import Arkham.Treachery.CardDefs.Import

aquaticAmbush :: CardDef
aquaticAmbush =
  (treachery "07185" "Aquatic Ambush" DevilReef 2)
    { cdCardTraits = singleton Scheme
    }

draggedUnder :: CardDef
draggedUnder =
  (treachery "07188" "Dragged Under" DevilReef 3)
    { cdCardTraits = setFromList [Scheme, Terror]
    }

horrorsFromTheDeep :: CardDef
horrorsFromTheDeep =
  (treachery "07186" "Horrors from the Deep" DevilReef 2)
    { cdCardTraits = singleton Terror
    }

shapesInTheWater :: CardDef
shapesInTheWater =
  (treachery "07184" "Shapes in the Water" DevilReef 2)
    { cdCardTraits = singleton Terror
    }

stowaway :: CardDef
stowaway =
  (treachery "07187" "Stowaway" DevilReef 2)
    { cdCardTraits = singleton Scheme
    }
