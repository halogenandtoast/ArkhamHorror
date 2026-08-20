module Arkham.Treachery.CardDefs.TheCircleUndone.TheWatcher where

import Arkham.Treachery.CardDefs.Import

watchersGrasp :: CardDef
watchersGrasp =
  (treachery "05087" "Watcher's Grasp" TheWatcher 2)
    { cdCardTraits = setFromList [Power, Spectral]
    }
