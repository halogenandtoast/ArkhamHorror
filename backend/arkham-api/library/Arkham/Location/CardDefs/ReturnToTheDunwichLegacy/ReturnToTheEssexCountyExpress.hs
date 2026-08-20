module Arkham.Location.CardDefs.ReturnToTheDunwichLegacy.ReturnToTheEssexCountyExpress where

import Arkham.Location.CardDefs.Import

baggageCar :: CardDef
baggageCar =
  locationWithUnrevealed_
    "51030"
    "Train Car"
    [Train]
    "Baggage Car"
    [Train]
    ReturnToTheEssexCountyExpress

freightCar :: CardDef
freightCar =
  locationWithUnrevealed_
    "51029"
    "Train Car"
    [Train]
    "Freight Car"
    [Train]
    ReturnToTheEssexCountyExpress

returnToEngineCar :: CardDef
returnToEngineCar =
  victory 1 $ location "51028" "Engine Car" [Train] NoSymbol [] ReturnToTheEssexCountyExpress
