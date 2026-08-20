module Arkham.Treachery.CardDefs.EdgeOfTheEarth.ElderThings where

import Arkham.Treachery.CardDefs.Import

riseOfTheElderThings :: CardDef
riseOfTheElderThings =
  (treachery "08697" "Rise of the Elder Things" ElderThings 2)
    { cdCardTraits = setFromList [Hazard]
    }
