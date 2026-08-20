module Arkham.Location.CardDefs.TheScarletKeys.SanguineShadows where

import Arkham.Location.CardDefs.Import

avenidaDeMayo :: CardDef
avenidaDeMayo =
  location
    "09549"
    "Avenida de Mayo"
    [BuenosAires, Central]
    Equals
    [Moon, T, Square, Circle]
    SanguineShadows

bancoDeLaProvincia :: CardDef
bancoDeLaProvincia =
  location
    "09555"
    "Banco de la Provincia"
    [BuenosAires]
    Square
    [Equals, Square]
    SanguineShadows

casaRosada :: CardDef
casaRosada =
  location
    "09550"
    "Casa Rosada"
    [BuenosAires]
    Moon
    [Equals, Moon]
    SanguineShadows

catedralMetropolitana :: CardDef
catedralMetropolitana =
  location
    "09551"
    "Catedral Metropolitana"
    [BuenosAires]
    Moon
    [Equals, Moon]
    SanguineShadows

cementarioDeLaRecoleta :: CardDef
cementarioDeLaRecoleta =
  location
    "09552"
    "Cementario de la Recoleta"
    [BuenosAires]
    T
    [Equals, T]
    SanguineShadows

palacioErrazuriz :: CardDef
palacioErrazuriz =
  location
    "09553"
    "Palacio Errázuriz"
    [BuenosAires]
    T
    [Equals, T]
    SanguineShadows

teatroColon :: CardDef
teatroColon =
  location
    "09556"
    "Teatro Colón"
    [BuenosAires]
    Circle
    [Equals]
    SanguineShadows

theCabildo :: CardDef
theCabildo =
  location
    "09554"
    "The Cabildo"
    [BuenosAires]
    Square
    [Equals, Square]
    SanguineShadows
