module Arkham.Treachery.CardDefs.TheDrownedCity.TheInescapable where

import Arkham.Treachery.CardDefs.Import

stillBehindYou :: CardDef
stillBehindYou =
  (treachery "11745" "Still Behind You" TheInescapable 3) {cdCardTraits = setFromList [Scheme]}
