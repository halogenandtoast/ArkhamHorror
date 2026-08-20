module Arkham.Treachery.CardDefs.EdgeOfTheEarth.CreaturesInTheIce where

import Arkham.Treachery.CardDefs.Import

kindredMist :: CardDef
kindredMist =
  (treachery "08691" "Kindred Mist" CreaturesInTheIce 2)
    { cdCardTraits = setFromList [Curse]
    }
