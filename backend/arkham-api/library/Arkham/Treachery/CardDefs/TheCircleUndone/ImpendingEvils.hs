module Arkham.Treachery.CardDefs.TheCircleUndone.ImpendingEvils where

import Arkham.Treachery.CardDefs.Import

impendingEvils :: CardDef
impendingEvils =
  peril
    (treachery "54065" "Impending Evils" ImpendingEvils 3)
      { cdCardTraits = setFromList [Omen]
      }
