module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Refractions where

import Arkham.Treachery.CardDefs.Import

captivatingGleam :: CardDef
captivatingGleam =
  surge
    $ (treachery "10731" "Captivating Gleam" Refractions 2)
      { cdCardTraits = setFromList [Power, Colour]
      }

empyreanBrilliance :: CardDef
empyreanBrilliance =
  (treachery "10730" "Empyrean Brilliance" Refractions 2)
    { cdCardTraits = setFromList [Power, Colour]
    }
