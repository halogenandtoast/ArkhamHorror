module Arkham.Treachery.CardDefs.TheDrownedCity.TheGrandVault where

import Arkham.Treachery.CardDefs.Import

ancientVaultN :: CardDef
ancientVaultN =
  (treachery "11609" "Ancient Vault" TheGrandVault 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdOtherSide = Just "11609b"
    , cdDoubleSided = True
    }

ancientVaultO :: CardDef
ancientVaultO =
  (treachery "11608" "Ancient Vault" TheGrandVault 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdOtherSide = Just "11608b"
    , cdDoubleSided = True
    }

ancientVaultP :: CardDef
ancientVaultP =
  (treachery "11610" "Ancient Vault" TheGrandVault 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdOtherSide = Just "11610b"
    , cdDoubleSided = True
    }

deadlyMechanisms :: CardDef
deadlyMechanisms =
  (treachery "11607" "Deadly Mechanisms" TheGrandVault 3) {cdCardTraits = setFromList [Hazard]}
