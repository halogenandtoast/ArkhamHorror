module Arkham.Treachery.CardDefs.TheBlobThatAteEverything where

import Arkham.Treachery.CardDefs.Import

alienFoodChain :: CardDef
alienFoodChain =
  (treachery "85053" "Alien Food Chain" TheBlobThatAteEverything 2)
    { cdCardTraits = setFromList [Ooze, Power]
    }

causticDissemination :: CardDef
causticDissemination =
  (treachery "85049" "Caustic Dissemination" TheBlobThatAteEverything 2)
    { cdCardTraits = setFromList [Ooze, Hazard]
    }

consumingMaw :: CardDef
consumingMaw =
  (treachery "85047" "Consuming Maw" TheBlobThatAteEverything 3)
    { cdCardTraits = singleton Attack
    }

corrosiveSlime :: CardDef
corrosiveSlime =
  (treachery "85046" "Corrosive Slime" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Attack
    }

devouringOoze :: CardDef
devouringOoze =
  (treachery "85045" "Devouring Ooze" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Attack
    }

itsGotMe :: CardDef
itsGotMe =
  (treachery "85052" "\"It's got me!\"" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Hazard
    }

realityAcid :: CardDef
realityAcid =
  (treachery "85044" "Reality Acid" TheBlobThatAteEverything 4)
    { cdCardTraits = singleton Power
    }

replication :: CardDef
replication =
  (treachery "85051" "Replication" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Power
    }

stickyFeet :: CardDef
stickyFeet =
  (treachery "85050" "Sticky Feet" TheBlobThatAteEverything 2)
    { cdCardTraits = setFromList [Ooze, Obstacle]
    }

waveOfOoze :: CardDef
waveOfOoze =
  (treachery "85048" "Wave of Ooze" TheBlobThatAteEverything 3)
    { cdCardTraits = singleton Attack
    }
