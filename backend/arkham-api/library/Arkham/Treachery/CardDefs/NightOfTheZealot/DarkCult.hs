module Arkham.Treachery.CardDefs.NightOfTheZealot.DarkCult where

import Arkham.Treachery.CardDefs.Import

mysteriousChanting :: CardDef
mysteriousChanting =
  (treachery "01171" "Mysterious Chanting" DarkCult 2)
    { cdCardTraits = setFromList [Hex]
    }
