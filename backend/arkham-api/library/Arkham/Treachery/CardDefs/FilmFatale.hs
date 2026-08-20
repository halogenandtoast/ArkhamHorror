module Arkham.Treachery.CardDefs.FilmFatale where

import Arkham.Treachery.CardDefs.Import

action :: CardDef
action =
  peril
    $ (treachery "72014" "Action!" FilmFatale 2)
      { cdCardTraits = singleton Terror
      }

bleedingReality :: CardDef
bleedingReality =
  (treachery "72016" "Bleeding Reality" FilmFatale 3)
    { cdCardTraits = setFromList [Power, Extradimensional]
    }

breakALeg :: CardDef
breakALeg =
  (treachery "72015" "\"Break a Leg!\"" FilmFatale 2)
    { cdCardTraits = singleton Hazard
    }

celestialShower :: CardDef
celestialShower =
  (treachery "72035" "Celestial Shower" CosmicJourney 2)
    { cdCardTraits = singleton Hazard
    }

creatureFeature :: CardDef
creatureFeature =
  (treachery "72018" "Creature Feature" FilmFatale 2)
    { cdCardTraits = singleton Terror
    }

flipTheScript :: CardDef
flipTheScript =
  (treachery "72012" "Flip the Script" FilmFatale 2)
    { cdCardTraits = singleton Paradox
    }

foundFootage :: CardDef
foundFootage =
  (treachery "72013" "Found Footage" FilmFatale 2)
    { cdCardTraits = singleton Terror
    }

hellfire :: CardDef
hellfire =
  (treachery "72058" "Hellfire" AbominableContessa 2)
    { cdCardTraits = singleton Power
    }

lastLooks :: CardDef
lastLooks =
  (treachery "72017" "Last Looks" FilmFatale 2)
    { cdCardTraits = singleton Terror
    }

primordialTerror :: CardDef
primordialTerror =
  (treachery "72046" "Primordial Terror" ForgottenIsland 2)
    { cdCardTraits = singleton Terror
    }

unexpectedTransformation :: CardDef
unexpectedTransformation =
  (treachery "72047" "Unexpected Transformation" ForgottenIsland 2)
    { cdCardTraits = singleton Power
    }

vampiresKiss :: CardDef
vampiresKiss =
  (treachery "72059" "Vampire's Kiss" AbominableContessa 2)
    { cdCardTraits = singleton Scheme
    }
