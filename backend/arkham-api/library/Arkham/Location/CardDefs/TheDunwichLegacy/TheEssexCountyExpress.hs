{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDunwichLegacy.TheEssexCountyExpress where

import Arkham.Location.CardDefs.Import

diningCar :: CardDef
diningCar =
  locationWithUnrevealed
    "02173"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Dining Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

engineCar_175 :: CardDef
engineCar_175 =
  victory 1
    $ location "02175" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_176 :: CardDef
engineCar_176 =
  victory 1
    $ location "02176" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_177 :: CardDef
engineCar_177 =
  victory 1
    $ location "02177" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

parlorCar :: CardDef
parlorCar =
  victory 1
    $ locationWithUnrevealed
      "02174"
      "Train Car"
      [Train]
      NoSymbol
      []
      "Parlor Car"
      [Train]
      NoSymbol
      []
      TheEssexCountyExpress

passengerCar_167 :: CardDef
passengerCar_167 =
  locationWithUnrevealed
    "02167"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_168 :: CardDef
passengerCar_168 =
  locationWithUnrevealed
    "02168"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_169 :: CardDef
passengerCar_169 =
  locationWithUnrevealed
    "02169"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_170 :: CardDef
passengerCar_170 =
  locationWithUnrevealed
    "02170"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_171 :: CardDef
passengerCar_171 =
  locationWithUnrevealed
    "02171"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

sleepingCar :: CardDef
sleepingCar =
  locationWithUnrevealed
    "02172"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Sleeping Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress
