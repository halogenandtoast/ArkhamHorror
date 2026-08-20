module Arkham.Treachery.CardDefs.TheMidwinterGala where

import Arkham.Treachery.CardDefs.Import

bleedingWalls :: CardDef
bleedingWalls =
  (treachery "71054" "Bleeding Walls" TheMidwinterGala 2)
    { cdCardTraits = singleton Terror
    }

coldStreak :: CardDef
coldStreak =
  surge
    $ (treachery "71032" "Cold Streak" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Misfortune, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

confusion :: CardDef
confusion =
  surge
    $ (treachery "71026" "Confusion" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Blunder, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

entrap :: CardDef
entrap =
  (treachery "71055" "Entrap" TheMidwinterGala 3)
    { cdCardTraits = singleton Hazard
    }

inexplicableCold :: CardDef
inexplicableCold =
  (treachery "71056" "Inexplicable Cold" TheMidwinterGala 1)
    { cdCardTraits = singleton Hazard
    }

mindExtraction :: CardDef
mindExtraction =
  (treachery "71057" "Mind Extraction" TheMidwinterGala 3)
    { cdCardTraits = singleton Power
    }

noxiousFumes :: CardDef
noxiousFumes =
  (treachery "71058" "Noxious Fumes" TheMidwinterGala 1)
    { cdCardTraits = singleton Hazard
    }

pushedIntoTheBeyond :: CardDef
pushedIntoTheBeyond =
  (treachery "71059" "Pushed into the Beyond" TheMidwinterGala 1)
    { cdCardTraits = singleton Hex
    }

secretDoor :: CardDef
secretDoor =
  (treachery "71060" "Secret Door" TheMidwinterGala 2)
    { cdCardTraits = singleton Obstacle
    }

terrorGate :: CardDef
terrorGate =
  (treachery "71061" "Terror Gate" TheMidwinterGala 4)
    { cdCardTraits = singleton Terror
    }

unlucky :: CardDef
unlucky =
  surge
    $ (treachery "71044" "Unlucky" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Misfortune, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

viciousAmbush :: CardDef
viciousAmbush =
  (treachery "71050" "Vicious Ambush" TheMidwinterGala 2)
    { cdCardTraits = singleton Scheme
    }

violentCommands :: CardDef
violentCommands =
  (treachery "71062" "Violent Commands" TheMidwinterGala 2)
    { cdCardTraits = singleton Terror
    }

wardOfPreservation :: CardDef
wardOfPreservation =
  surge
    $ (treachery "71038" "Ward of Preservation" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Spell, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }
