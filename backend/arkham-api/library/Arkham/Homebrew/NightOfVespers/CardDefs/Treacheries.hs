module Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries where

import Arkham.Homebrew.NightOfVespers.Sets qualified as Set
import Arkham.Treachery.CardDefs.Import

inHotPursuit :: CardDef
inHotPursuit =
  (treachery ":night-of-vespers:034" "In Hot Pursuit" Set.TheEndOfSeasons 2)
    { cdCardTraits = setFromList [Scheme]
    }

secondThoughts :: CardDef
secondThoughts =
  (treachery ":night-of-vespers:035" "Second Thoughts" Set.TheEndOfSeasons 2)
    { cdCardTraits = setFromList [Terror]
    }

rainOfBullets :: CardDef
rainOfBullets =
  (treachery ":night-of-vespers:340" "Rain of Bullets" Set.TheFamily 2)
    { cdCardTraits = setFromList [Attack, Hazard]
    }

catastrophicDrop :: CardDef
catastrophicDrop =
  (treachery ":night-of-vespers:348" "Catastrophic Drop" Set.HostileArchitecture 2)
    { cdCardTraits = setFromList [Hazard]
    }

crumblingFoundations :: CardDef
crumblingFoundations =
  (treachery ":night-of-vespers:349" "Crumbling Foundations" Set.HostileArchitecture 2)
    { cdCardTraits = setFromList [Hazard]
    }

backdraft :: CardDef
backdraft =
  (treachery ":night-of-vespers:350" "Backdraft" Set.Inferno 2)
    { cdCardTraits = setFromList [Hazard]
    }

conflagration :: CardDef
conflagration =
  (treachery ":night-of-vespers:351" "Conflagration" Set.Inferno 2)
    { cdCardTraits = setFromList [Hazard]
    }

kenophobia :: CardDef
kenophobia =
  (treachery ":night-of-vespers:355" "Kenophobia" Set.LiminalSpaces 2)
    { cdCardTraits = setFromList [Terror]
    }

liminalDread :: CardDef
liminalDread =
  (treachery ":night-of-vespers:356" "Liminal Dread" Set.LiminalSpaces 2)
    { cdCardTraits = setFromList [Power]
    }
