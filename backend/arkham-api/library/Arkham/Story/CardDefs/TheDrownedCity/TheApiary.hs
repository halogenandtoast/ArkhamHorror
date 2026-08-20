module Arkham.Story.CardDefs.TheDrownedCity.TheApiary where

import Arkham.Story.CardDefs.Import

ancientRelic :: CardDef
ancientRelic =
  (doubleSided $ story "11581b" "Ancient Relic" TheApiary)
    { cdVictoryPoints = Just 1
    }

hiddenVault :: CardDef
hiddenVault = doubleSided $ story "11579b" "Hidden Vault" TheApiary

squamousParasite :: CardDef
squamousParasite =
  (doubleSided $ story "11580b" "Squamous Parasite" TheApiary)
    { cdVictoryPoints = Just 1
    }
