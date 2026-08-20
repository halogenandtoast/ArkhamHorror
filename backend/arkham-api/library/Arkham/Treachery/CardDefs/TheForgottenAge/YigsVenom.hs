module Arkham.Treachery.CardDefs.TheForgottenAge.YigsVenom where

import Arkham.Treachery.CardDefs.Import

serpentsCall :: CardDef
serpentsCall =
  (treachery "04100" "Serpent's Call" YigsVenom 1)
    { cdCardTraits = singleton Power
    }

snakescourge :: CardDef
snakescourge =
  (treachery "04099" "Snakescourge" YigsVenom 2)
    { cdCardTraits = singleton Curse
    }
