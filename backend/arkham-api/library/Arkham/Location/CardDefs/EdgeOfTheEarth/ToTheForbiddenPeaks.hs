module Arkham.Location.CardDefs.EdgeOfTheEarth.ToTheForbiddenPeaks where

import Arkham.Location.CardDefs.Import

ancientPlanetarium :: CardDef
ancientPlanetarium =
  locationWithUnrevealed_
    "08631"
    "City Landscape"
    [City]
    "Ancient Planetarium"
    [City]
    ToTheForbiddenPeaks

cyclopeanSpires :: CardDef
cyclopeanSpires =
  locationWithUnrevealed_
    "08638"
    "City Landscape"
    [City]
    "Cyclopean Spires"
    [City]
    ToTheForbiddenPeaks

cylindricalTower :: CardDef
cylindricalTower =
  locationWithUnrevealed_
    "08633"
    "City Landscape"
    [City]
    "Cylindrical Tower"
    [City]
    ToTheForbiddenPeaks

deepDrifts :: CardDef
deepDrifts =
  victory 1
    $ locationWithUnrevealed
      "08600"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Deep Drifts"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

labyrinthineChamber :: CardDef
labyrinthineChamber =
  quantity 2
    $ victory 1
    $ locationWithUnrevealed_
      "08634"
      "City Landscape"
      [City]
      "Labyrinthine Chamber"
      [City]
      ToTheForbiddenPeaks

mapRoom :: CardDef
mapRoom =
  locationWithUnrevealed_
    "08635"
    "City Landscape"
    [City]
    "Map Room"
    [City]
    ToTheForbiddenPeaks

narrowRidge :: CardDef
narrowRidge =
  victory 1
    $ locationWithUnrevealed
      "08603"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Narrow Ridge"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

rockyLedge :: CardDef
rockyLedge =
  victory 1
    $ locationWithUnrevealed
      "08604"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Rocky Ledge"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

rooflessRampart :: CardDef
rooflessRampart =
  locationWithUnrevealed_
    "08636"
    "City Landscape"
    [City]
    "Roofless Rampart"
    [City]
    ToTheForbiddenPeaks

ruinousStreets :: CardDef
ruinousStreets =
  quantity 2
    $ locationWithUnrevealed_
      "08637"
      "City Landscape"
      [City]
      "Ruinous Streets"
      [City]
      ToTheForbiddenPeaks

snowCoveredCrag :: CardDef
snowCoveredCrag =
  victory 1
    $ locationWithUnrevealed
      "08605"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Snow-Covered Crag"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

steepIncline :: CardDef
steepIncline =
  victory 1
    $ locationWithUnrevealed
      "08602"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Steep Incline"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

stoneBridge :: CardDef
stoneBridge =
  quantity 3
    $ locationWithUnrevealed_
      "08632"
      "City Landscape"
      [City]
      "Stone Bridge"
      [City]
      ToTheForbiddenPeaks

submergedPassageway :: CardDef
submergedPassageway =
  quantity 2
    $ locationWithUnrevealed_
      "08639"
      "City Landscape"
      [City]
      "Submerged Passageway"
      [City]
      ToTheForbiddenPeaks

templeOfTheElderThings :: CardDef
templeOfTheElderThings =
  quantity 2
    $ locationWithUnrevealed_
      "08640"
      "City Landscape"
      [City]
      "Temple of the Elder Things"
      [City]
      ToTheForbiddenPeaks

theSummit :: CardDef
theSummit =
  victory 1
    $ location
      "08607"
      "The Summit"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

whiteBluff :: CardDef
whiteBluff =
  victory 1
    $ locationWithUnrevealed
      "08601"
      "Mountainside"
      mempty
      NoSymbol
      []
      "White Bluff"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

windsweptPath :: CardDef
windsweptPath =
  victory 1
    $ locationWithUnrevealed
      "08606"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Windswept Path"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks
