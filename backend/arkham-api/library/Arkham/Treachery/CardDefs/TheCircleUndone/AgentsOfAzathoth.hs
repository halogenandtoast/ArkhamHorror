module Arkham.Treachery.CardDefs.TheCircleUndone.AgentsOfAzathoth where

import Arkham.Treachery.CardDefs.Import

daemonicPiping :: CardDef
daemonicPiping =
  surge
    $ (treachery "05089" "Daemonic Piping" AgentsOfAzathoth 3)
      { cdCardTraits = setFromList [Power, Terror]
      }
