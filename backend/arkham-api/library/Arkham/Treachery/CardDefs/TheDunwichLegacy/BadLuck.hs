module Arkham.Treachery.CardDefs.TheDunwichLegacy.BadLuck where

import Arkham.Treachery.CardDefs.Import

cursedLuck :: CardDef
cursedLuck =
  (treachery "02092" "Cursed Luck" BadLuck 3)
    { cdCardTraits = setFromList [Omen]
    }

twistOfFate :: CardDef
twistOfFate =
  (treachery "02093" "Twist of Fate" BadLuck 3)
    { cdCardTraits = setFromList [Omen]
    }
