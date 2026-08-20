module Arkham.Treachery.CardDefs.EdgeOfTheEarth.CityOfTheElderThings where

import Arkham.Treachery.CardDefs.Import

crumblingRuins :: CardDef
crumblingRuins =
  (treachery "08645" "Crumbling Ruins" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Hazard]
    }

dawningOfTheTruth :: CardDef
dawningOfTheTruth =
  (treachery "08644" "Dawning of the Truth" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Terror]
    }

frostbitten :: CardDef
frostbitten =
  (weakness "08646" "Frostbitten")
    { cdCardTraits = setFromList [Injury]
    , cdEncounterSet = Just CityOfTheElderThings
    , cdEncounterSetQuantity = Just 4
    }

possessed :: CardDef
possessed =
  (weakness "08647" "Possessed")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just CityOfTheElderThings
    , cdEncounterSetQuantity = Just 4
    }
