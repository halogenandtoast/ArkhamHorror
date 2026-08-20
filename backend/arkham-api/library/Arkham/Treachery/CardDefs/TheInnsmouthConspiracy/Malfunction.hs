module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.Malfunction where

import Arkham.Treachery.CardDefs.Import

malfunction :: CardDef
malfunction =
  (treachery "07099" "Malfunction" Malfunction 2)
    { cdCardTraits = singleton Blunder
    }
