module Arkham.Treachery.CardDefs.TheDrownedCity.DeepOnes where

import Arkham.Treachery.CardDefs.Import

deepOneAmbush :: CardDef
deepOneAmbush =
  (treachery "11748" "Deep One Ambush" DeepOnes 2) {cdCardTraits = setFromList [Scheme]}
