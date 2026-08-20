module Arkham.Treachery.CardDefs.TheDreamEaters.WeaverOfTheCosmos where

import Arkham.Treachery.CardDefs.Import

caughtInAWeb :: CardDef
caughtInAWeb =
  (treachery "06353" "Caught in a Web" WeaverOfTheCosmos 3)
    { cdCardTraits = singleton Hazard
    }

endlessWeaving :: CardDef
endlessWeaving =
  (treachery "06354" "Endless Weaving" WeaverOfTheCosmos 3)
    { cdCardTraits = singleton Scheme
    }

sickeningWebs :: CardDef
sickeningWebs =
  (treachery "06103" "Sickening Webs" Spiders 2)
    { cdCardTraits = singleton Obstacle
    }

theSpinnerInDarkness :: CardDef
theSpinnerInDarkness =
  (treachery "06352" "The Spinner in Darkness" WeaverOfTheCosmos 2)
    { cdCardTraits = singleton Power
    }
