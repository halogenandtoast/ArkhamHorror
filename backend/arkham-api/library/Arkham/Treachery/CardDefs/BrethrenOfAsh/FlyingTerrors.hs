module Arkham.Treachery.CardDefs.BrethrenOfAsh.FlyingTerrors where

import Arkham.Treachery.CardDefs.Import

aerialPursuit :: CardDef
aerialPursuit =
  surge
    $ (treachery "12163" "Aerial Pursuit" FlyingTerrors 2)
      { cdCardTraits = singleton Scheme
      }
