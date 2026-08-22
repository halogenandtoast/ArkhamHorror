module Arkham.Treachery.CardDefs.ChildrenOfBlood.NewHorizons where

import Arkham.Treachery.CardDefs.Import

echoingInDarkness :: CardDef
echoingInDarkness =
  (treachery "13065" "Echoing in Darkness" NewHorizons 4)
    { cdCardTraits = singleton Terror
    }
