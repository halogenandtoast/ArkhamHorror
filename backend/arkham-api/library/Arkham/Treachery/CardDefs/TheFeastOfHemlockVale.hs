module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale where

import Arkham.Treachery.CardDefs.Import
import Arkham.Trait qualified as Trait

hastyRepairs :: CardDef
hastyRepairs =
  (weakness "10003" "Hasty Repairs")
    { cdCardTraits = setFromList [Blunder]
    }

failedExperiment :: CardDef
failedExperiment =
  (weakness "10008" "Failed Experiment")
    { cdCardTraits = setFromList [Blunder]
    }

wheresPa :: CardDef
wheresPa =
  (weakness "10018" "\"Where's Pa?\"")
    { cdCardTraits = setFromList [Flaw]
    }

caveIn :: CardDef
caveIn =
  (treachery "10520" "Cave-In" WrittenInRock 2)
    { cdCardTraits = setFromList [Hazard]
    }

wildRide :: CardDef
wildRide =
  (treachery "10521" "Wild Ride" WrittenInRock 3)
    { cdCardTraits = setFromList [Hazard]
    }

deepShadows :: CardDef
deepShadows =
  (treachery "10622" "Deep Shadows" TheTwistedHollow 2)
    { cdCardTraits = setFromList [Hazard]
    }

lurkingFear :: CardDef
lurkingFear =
  (treachery "10623" "Lurking Fear" TheTwistedHollow 2)
    { cdCardTraits = setFromList [Terror]
    }

stolenLight :: CardDef
stolenLight =
  peril
    $ (treachery "10624" "Stolen Light" TheTwistedHollow 1)
      { cdCardTraits = setFromList [Scheme]
      }

swarm :: CardDef
swarm =
  (treachery "10676" "Swarm" TheFirstDay 3)
    { cdCardTraits = setFromList [Hazard]
    }

downpour :: CardDef
downpour =
  (treachery "10678" "Downpour" TheSecondDay 3)
    { cdCardTraits = setFromList [Hazard]
    }

chromaBlight :: CardDef
chromaBlight =
  (treachery "10722" "Chroma Blight" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Power, Trait.Blight]
    }

calcification :: CardDef
calcification =
  (treachery "10723" "Calcification" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }

empyreanBrilliance :: CardDef
empyreanBrilliance =
  (treachery "10730" "Empyrean Brilliance" Refractions 2)
    { cdCardTraits = setFromList [Power, Colour]
    }

captivatingGleam :: CardDef
captivatingGleam =
  surge
    $ (treachery "10731" "Captivating Gleam" Refractions 2)
      { cdCardTraits = setFromList [Power, Colour]
      }

bloom :: CardDef
bloom =
  (treachery "10735" "Bloom" TheForest 2)
    { cdCardTraits = setFromList [Power]
    }

wallOfThorns :: CardDef
wallOfThorns =
  (treachery "10736" "Wall of Thorns" TheForest 2)
    { cdCardTraits = setFromList [Hazard, Flora]
    }

callOfTheWild :: CardDef
callOfTheWild =
  (treachery "10737" "Call of the Wild" TheForest 2)
    { cdCardTraits = setFromList [Terror]
    }

psychotropicSpores :: CardDef
psychotropicSpores =
  (treachery "10740" "Psychotropic Spores" Myconids 2)
    { cdCardTraits = setFromList [Hazard, Flora]
    }
