module Arkham.Treachery.CardDefs.TheCircleUndone.Hexcraft where

import Arkham.Treachery.CardDefs.Import

despoiled :: CardDef
despoiled =
  (treachery "54063" "Despoiled" Hexcraft 2)
    { cdCardTraits = setFromList [Hex]
    }

maligned :: CardDef
maligned =
  (treachery "54064" "Maligned" Hexcraft 2)
    { cdCardTraits = setFromList [Hex]
    }

trespasser :: CardDef
trespasser =
  (treachery "54062" "Trespasser!" Hexcraft 3)
    { cdCardTraits = setFromList [Curse]
    }
