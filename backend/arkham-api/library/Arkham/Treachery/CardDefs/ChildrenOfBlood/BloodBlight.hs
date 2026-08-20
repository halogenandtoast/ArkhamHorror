module Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodBlight where

import Arkham.Treachery.CardDefs.Import

blightedBlood :: CardDef
blightedBlood =
  (treachery "13099" "Blighted Blood" BloodBlight 2)
    { cdCardTraits = setFromList [Curse, Blight]
    }

grislyCompulsion :: CardDef
grislyCompulsion =
  (treachery "13100" "Grisly Compulsion" BloodBlight 3)
    { cdCardTraits = singleton Power
    }
