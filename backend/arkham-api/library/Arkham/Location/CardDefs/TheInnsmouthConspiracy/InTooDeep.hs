module Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep where

import Arkham.Location.CardDefs.Import

desolateCoastline :: CardDef
desolateCoastline = location "07143" "Desolate Coastline" [Innsmouth, Coastal] NoSymbol [] InTooDeep

esotericOrderOfDagonInTooDeep :: CardDef
esotericOrderOfDagonInTooDeep =
  location
    "07129"
    "Esoteric Order of Dagon"
    [Innsmouth, Midtown]
    NoSymbol
    []
    InTooDeep

firstNationalGrocery :: CardDef
firstNationalGrocery =
  victory 1 $ location "07137" "First National Grocery" [Innsmouth, Midtown] NoSymbol [] InTooDeep

fishStreetBridge :: CardDef
fishStreetBridge =
  victory 1
    $ location "07136" "Fish Street Bridge" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

gilmanHouse :: CardDef
gilmanHouse = location "07140" "Gilman House" [Innsmouth, Midtown] NoSymbol [] InTooDeep

innsmouthHarbourInTooDeep :: CardDef
innsmouthHarbourInTooDeep =
  location "07138" "Fish Street Bridge" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

innsmouthJailInTooDeep :: CardDef
innsmouthJailInTooDeep =
  victory 1
    $ location
      "07133"
      "Innsmouth Jail"
      [Innsmouth, Midtown]
      NoSymbol
      []
      InTooDeep

innsmouthSquare :: CardDef
innsmouthSquare =
  victory 1 $ location "07139" "Innsmouth Square" [Innsmouth, Midtown] NoSymbol [] InTooDeep

marshRefinery :: CardDef
marshRefinery = location "07141" "Marsh Refinery" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

newChurchGreenInTooDeep :: CardDef
newChurchGreenInTooDeep =
  location
    "07134"
    "New Church Green"
    [Innsmouth, Midtown]
    NoSymbol
    []
    InTooDeep

railroadStation :: CardDef
railroadStation = location "07142" "Railroad Station" [Innsmouth] NoSymbol [] InTooDeep

sawboneAlleyInTooDeep :: CardDef
sawboneAlleyInTooDeep =
  victory 1
    $ location
      "07130"
      "Sawbone Alley"
      [Innsmouth]
      NoSymbol
      []
      InTooDeep

shorewardSlumsInTooDeep :: CardDef
shorewardSlumsInTooDeep =
  location
    "07131"
    "Shoreward Slums"
    [Innsmouth, Coastal, Midtown]
    NoSymbol
    []
    InTooDeep

theHouseOnWaterStreetInTooDeep :: CardDef
theHouseOnWaterStreetInTooDeep =
  location
    "07132"
    "The House on Water Street"
    [Innsmouth, Coastal]
    NoSymbol
    []
    InTooDeep

theLittleBookshop :: CardDef
theLittleBookshop =
  location
    "07135"
    "The Little Bookshop"
    [Innsmouth]
    NoSymbol
    []
    InTooDeep
