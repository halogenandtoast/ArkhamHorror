module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheFinalDay where

import Arkham.Treachery.CardDefs.Import

otherworldlyVisions :: CardDef
otherworldlyVisions =
  (treachery "10680" "Otherworldly Visions" TheFinalDay 3)
    { cdCardTraits = setFromList [Terror, Power]
    }
