module Arkham.Story.CardDefs.TheDrownedCity.TheGrandVault where

import Arkham.Story.CardDefs.Import

ancientVaultN :: CardDef
ancientVaultN =
  (doubleSided $ story "11609b" "Ancient Vault" TheGrandVault)
    { cdVictoryPoints = Just 1
    }

ancientVaultO :: CardDef
ancientVaultO =
  (doubleSided $ story "11608b" "Ancient Vault" TheGrandVault)
    { cdVictoryPoints = Just 1
    }

ancientVaultP :: CardDef
ancientVaultP =
  (doubleSided $ story "11610b" "Ancient Vault" TheGrandVault)
    { cdVictoryPoints = Just 1
    }
