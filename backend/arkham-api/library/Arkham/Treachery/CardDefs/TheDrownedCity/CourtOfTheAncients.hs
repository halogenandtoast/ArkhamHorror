module Arkham.Treachery.CardDefs.TheDrownedCity.CourtOfTheAncients where

import Arkham.Treachery.CardDefs.Import

ancientVaultG :: CardDef
ancientVaultG =
  (treachery "11632" "Ancient Vault" CourtOfTheAncients 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdVictoryPoints = Just 1
    }

ancientVaultI :: CardDef
ancientVaultI =
  (treachery "11633" "Ancient Vault" CourtOfTheAncients 1)
    { cdCardTraits = setFromList [Rlyeh, Glyph]
    , cdVictoryPoints = Just 1
    }

cosmicOmen :: CardDef
cosmicOmen =
  (treachery "11636" "Cosmic Omen" CourtOfTheAncients 4)
    { cdCardTraits = setFromList [Omen, Power]
    }

ruinedOrrery :: CardDef
ruinedOrrery =
  (treachery "11634" "Ruined Orrery" CourtOfTheAncients 1)
    { cdCardTraits = setFromList [Glyph]
    , cdVictoryPoints = Just 1
    }
