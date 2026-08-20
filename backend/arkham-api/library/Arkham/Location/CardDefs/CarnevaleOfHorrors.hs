module Arkham.Location.CardDefs.CarnevaleOfHorrors where

import Arkham.Location.CardDefs.Import

accademiaBridge :: CardDef
accademiaBridge =
  location
    "82015"
    "Accademia Bridge"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

bridgeOfSighs :: CardDef
bridgeOfSighs =
  location
    "82013"
    "Bridge of Sighs"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

canalSide :: CardDef
canalSide =
  location "82009" "Canal-side" [Venice] NoSymbol [] CarnevaleOfHorrors

floodedSquare :: CardDef
floodedSquare =
  location "82014" "Flooded Square" [Venice] NoSymbol [] CarnevaleOfHorrors

gondola :: CardDef
gondola =
  location "82006b" "Gondola" [Venice, Boat] NoSymbol [] CarnevaleOfHorrors
    & otherSideIs "82006"

rialtoBridge :: CardDef
rialtoBridge =
  location
    "82011"
    "Rialto Bridge"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

sanMarcoBasilica :: CardDef
sanMarcoBasilica =
  location "82008" "San Marco Basilica" [Venice] NoSymbol [] CarnevaleOfHorrors

streetsOfVenice :: CardDef
streetsOfVenice =
  location "82010" "Streets of Venice" [Venice] NoSymbol [] CarnevaleOfHorrors

theGuardian :: CardDef
theGuardian =
  location "82016" "The Guardian" [Venice] NoSymbol [] CarnevaleOfHorrors

venetianGarden :: CardDef
venetianGarden =
  location "82012" "Venetian Garden" [Venice] NoSymbol [] CarnevaleOfHorrors
