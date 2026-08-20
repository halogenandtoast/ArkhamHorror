module Arkham.Treachery.CardDefs.BrethrenOfAsh.Fire1 where

import Arkham.Treachery.CardDefs.Import

fire1 :: CardDef
fire1 =
  (treachery "12129" "Fire!" Fire1 5)
    { cdCardTraits = singleton Hazard
    }

noxiousSmoke :: CardDef
noxiousSmoke =
  (treachery "12130" "Noxious Smoke" Fire1 2)
    { cdCardTraits = singleton Hazard
    }
