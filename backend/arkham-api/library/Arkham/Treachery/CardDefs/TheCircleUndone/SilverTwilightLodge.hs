module Arkham.Treachery.CardDefs.TheCircleUndone.SilverTwilightLodge where

import Arkham.Treachery.CardDefs.Import

mysteriesOfTheLodge :: CardDef
mysteriesOfTheLodge =
  (treachery "05097" "Mysteries of the Lodge" SilverTwilightLodge 2)
    { cdCardTraits = singleton Scheme
    }
