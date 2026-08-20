module Arkham.Treachery.CardDefs.ThePathToCarcosa.DelusoryEvils where

import Arkham.Treachery.CardDefs.Import

delusoryEvils :: CardDef
delusoryEvils =
  hidden
    $ peril
    $ (treachery "52065" "Delusory Evils" DelusoryEvils 3)
      { cdCardTraits = setFromList [Curse]
      }
