module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.HorrorInHighGear where

import Arkham.Treachery.CardDefs.Import

bumpyRide :: CardDef
bumpyRide =
  (treachery "07216" "Bumpy Ride" HorrorInHighGear 2)
    { cdCardTraits = singleton Hazard
    }

eyesInTheTrees :: CardDef
eyesInTheTrees =
  (treachery "07218" "Eyes in the Trees" HorrorInHighGear 2)
    { cdCardTraits = singleton Hazard
    }

iCantSee :: CardDef
iCantSee =
  (treachery "07217" "\"I can't see\"" HorrorInHighGear 2)
    { cdCardTraits = singleton Hazard
    }

theyreCatchingUp :: CardDef
theyreCatchingUp =
  (treachery "07219" "\"They're catching up!\"" HorrorInHighGear 2)
    { cdCardTraits = singleton Scheme
    }
