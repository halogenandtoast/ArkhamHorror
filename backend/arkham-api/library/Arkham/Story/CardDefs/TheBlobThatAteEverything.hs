module Arkham.Story.CardDefs.TheBlobThatAteEverything where

import Arkham.Story.CardDefs.Import

defuseTheExplosives :: CardDef
defuseTheExplosives =
  victory 1 $ addTrait Part1 $ doubleSided $ story "85024" "Defuse the Explosives" MiGoIncursion

driveOffTheMiGo :: CardDef
driveOffTheMiGo =
  victory 1 $ addTrait Part1 $ doubleSided $ story "85023" "Drive Off the Mi-Go" MiGoIncursion

escortTheCar :: CardDef
escortTheCar = victory 1 $ addTrait Part1 $ doubleSided $ story "89011" "Escort the Car" MiGoIncursionII

preventTheirResearch :: CardDef
preventTheirResearch = victory 1 $ addTrait Part1 $ doubleSided $ story "89017" "Prevent Their Research" MiGoIncursionII

reclaimTheBrain :: CardDef
reclaimTheBrain = victory 1 $ addTrait Part1 $ doubleSided $ story "89014" "Reclaim the Brain" MiGoIncursionII

recoverTheSample :: CardDef
recoverTheSample =
  victory 1 $ addTrait Part1 $ doubleSided $ story "85022" "Recover the Sample" MiGoIncursion

rescueTheChemist :: CardDef
rescueTheChemist =
  victory 1 $ addTrait Part1 $ doubleSided $ story "85021" "Rescue the Chemist" MiGoIncursion
