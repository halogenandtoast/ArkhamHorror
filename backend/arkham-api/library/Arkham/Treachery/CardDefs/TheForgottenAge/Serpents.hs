module Arkham.Treachery.CardDefs.TheForgottenAge.Serpents where

import Arkham.Treachery.CardDefs.Import

snakeBite :: CardDef
snakeBite =
  (treachery "04080" "Snake Bite" Serpents 3)
    { cdCardTraits = setFromList [Hazard, Poison]
    }
