{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.EdgeOfTheEarth where

import Arkham.Location.CardDefs.Import

crashSite :: CardDef
crashSite =
  withMeta ("shelter", Number 0)
    $ location "08502" "Crash Site" mempty Circle [Diamond, Triangle, Heart] IceAndDeath

frozenShores :: CardDef
frozenShores =
  withMeta ("shelter", Number 2)
    $ location "08503" "Frozen Shores" [Mainland] Diamond [Circle, Triangle, Heart, Square] IceAndDeath

treacherousPath :: CardDef
treacherousPath =
  withMeta ("shelter", Number 1)
    $ location
      "08504"
      "Treacherous Path"
      [Mountains]
      Triangle
      [Circle, Diamond, Heart, Equals]
      IceAndDeath

precariousIceSheet :: CardDef
precariousIceSheet =
  withMeta ("shelter", Number 2)
    $ location
      "08505"
      "Precarious Ice Sheet"
      [Glacier]
      Heart
      [Circle, Diamond, Triangle, Droplet]
      IceAndDeath

broadSnowdrifts :: CardDef
broadSnowdrifts =
  withMeta ("shelter", Number 4)
    $ location
      "08506"
      "Broad Snowdrifts"
      [Mainland, Uncharted]
      Square
      [Diamond, Hourglass, Star, Squiggle]
      IceAndDeath

icyWastes :: CardDef
icyWastes =
  withMeta ("shelter", Number 4)
    $ location
      "08507"
      "Broad Snowdrifts"
      [Glacier, Uncharted]
      Droplet
      [Heart, Star, Moon, Trefoil]
      IceAndDeath

rockyCrags :: CardDef
rockyCrags =
  withMeta ("shelter", Number 3)
    $ location
      "08508"
      "Rocky Crags"
      [Mountains, Uncharted]
      Equals
      [Triangle, Hourglass, Moon, Plus]
      IceAndDeath

snowGraves :: CardDef
snowGraves =
  withMeta ("shelter", Number 5)
    $ location
      "08509"
      "Snow Graves"
      [Mainland, Uncharted]
      Squiggle
      [Square]
      IceAndDeath

icebreakerLanding :: CardDef
icebreakerLanding =
  withMeta ("shelter", Number 5)
    $ location
      "08510"
      "Icebreaker Landing"
      [Glacier, Uncharted]
      Trefoil
      [Droplet]
      IceAndDeath

frigidCave :: CardDef
frigidCave =
  withMeta ("shelter", Number 6)
    $ location
      "08511"
      "Frigid Cave"
      [Mountains, Uncharted]
      Plus
      [Equals]
      IceAndDeath

barrierCamp :: CardDef
barrierCamp =
  withMeta ("shelter", Number 7)
    $ location
      "08512"
      "Barrier Camp"
      [Glacier, Uncharted]
      Moon
      [Droplet, Equals]
      IceAndDeath

remnantsOfLakesCamp :: CardDef
remnantsOfLakesCamp =
  withMeta ("shelter", Number 7)
    $ location
      "08513"
      "Remnants of Lake's Camp"
      [Mainland, Uncharted]
      Star
      [Square, Droplet]
      IceAndDeath

crystallineCavern :: CardDef
crystallineCavern =
  withMeta ("shelter", Number 8)
    $ location
      "08514"
      "Crystalline Cavern"
      [Mountains, Uncharted]
      Hourglass
      [Equals, Square]
      IceAndDeath

prisonOfMemories :: CardDef
prisonOfMemories =
  otherSideIs "08556b"
    $ location
      "08556"
      "Prison of Memories"
      [Otherworld, Mirage]
      Circle
      [Triangle]
      FatalMirage

baseCamp :: CardDef
baseCamp =
  otherSideIs "08557b"
    $ location
      "08557"
      "Base Camp"
      [Mirage]
      Triangle
      [Circle, Triangle, Trefoil, Square, Spade]
      FatalMirage

deckOfTheTheodosia :: CardDef
deckOfTheTheodosia =
  otherSideIs "08558b"
    $ location
      "08558"
      "Deck of the Theodosia"
      [Mirage]
      Triangle
      [Circle, Triangle, Hourglass, Square, Diamond]
      FatalMirage

universityHalls :: CardDef
universityHalls =
  otherSideIs "08559b"
    $ location
      "08559"
      "University Halls"
      [Mirage]
      Triangle
      [Circle, Triangle, Moon, Trefoil, Hourglass]
      FatalMirage

hedgeMaze :: CardDef
hedgeMaze =
  otherSideIs "08560b"
    $ location
      "08560"
      "Hedge Maze"
      [Mirage]
      Diamond
      [Circle, Equals]
      FatalMirage

desertedStation :: CardDef
desertedStation =
  otherSideIs "08561b"
    $ location
      "08561"
      "Deserted Station"
      [Mirage]
      Spade
      [Circle, Heart]
      FatalMirage

coastalWaters :: CardDef
coastalWaters =
  otherSideIs "08562b"
    $ location
      "08562"
      "Coastal Waters"
      [Mirage]
      Square
      [Circle, Star]
      FatalMirage

elderChamber :: CardDef
elderChamber =
  otherSideIs "08563b"
    $ location
      "08563"
      "Elder Chamber"
      [Mirage]
      Moon
      [Circle, T]
      FatalMirage

riverviewTheatre :: CardDef
riverviewTheatre =
  otherSideIs "08564b"
    $ location
      "08564"
      "Riverview Theatre"
      [Mirage]
      Trefoil
      [Circle, Squiggle]
      FatalMirage

standingStones :: CardDef
standingStones =
  otherSideIs "08565b"
    $ location
      "08565"
      "Standing Stones"
      [Mirage]
      Hourglass
      [Circle, Droplet]
      FatalMirage

airfield :: CardDef
airfield =
  otherSideIs "08566b"
    $ location
      "08566"
      "Airfield"
      [Mirage]
      Star
      [Circle]
      FatalMirage

alaskanWilds :: CardDef
alaskanWilds =
  otherSideIs "08567b"
    $ location
      "08567"
      "Alaskan Wilds"
      [Mirage]
      Heart
      [Circle]
      FatalMirage

clutteredDormitory :: CardDef
clutteredDormitory =
  otherSideIs "08568b"
    $ location
      "08568"
      "Cluttered Dormitory"
      [Mirage]
      T
      [Circle]
      FatalMirage

dyersClassroom :: CardDef
dyersClassroom =
  otherSideIs "08569b"
    $ location
      "08569"
      "Dyer's Classroom"
      [Mirage]
      Droplet
      [Circle]
      FatalMirage

infirmaryFatalMirage :: CardDef
infirmaryFatalMirage =
  otherSideIs "08570b"
    $ location
      "08570"
      "Infirmary"
      [Mirage]
      Squiggle
      [Circle]
      FatalMirage

drKenslersOffice :: CardDef
drKenslersOffice =
  otherSideIs "08571b"
    $ location
      "08571"
      "Dr. Kensler's Office"
      [Mirage]
      Squiggle
      [Circle]
      FatalMirage

moaiStatues :: CardDef
moaiStatues =
  otherSideIs "08572b"
    $ location
      "08572"
      "Mo'ai Statues"
      [Mirage]
      Equals
      [Circle]
      FatalMirage

ottomanFront :: CardDef
ottomanFront =
  otherSideIs "08573b"
    $ location
      "08573"
      "Ottoman Front"
      [Mirage]
      Star
      [Circle]
      FatalMirage

theBlackStone :: CardDef
theBlackStone =
  otherSideIs "08574b"
    $ location
      "08574"
      "The Black Stone"
      [Mirage]
      Droplet
      [Circle]
      FatalMirage

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

hiddenTunnelEntranceToTheDepths :: CardDef
hiddenTunnelEntranceToTheDepths =
  victory 1
    $ location
      "08630"
      ("Hidden Tunnel" <:> "Entrance to the Depths")
      [City]
      NoSymbol
      []
      CityOfTheElderThings

ancientPlanetarium :: CardDef
ancientPlanetarium =
  locationWithUnrevealed_
    "08631"
    "City Landscape"
    [City]
    "Ancient Planetarium"
    [City]
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

cylindricalTower :: CardDef
cylindricalTower =
  locationWithUnrevealed_
    "08633"
    "City Landscape"
    [City]
    "Cylindrical Tower"
    [City]
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

cyclopeanSpires :: CardDef
cyclopeanSpires =
  locationWithUnrevealed_
    "08638"
    "City Landscape"
    [City]
    "Cyclopean Spires"
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

theGateOfYquaa :: CardDef
theGateOfYquaa =
  location_
    "08649"
    ("The Gate of Y'quaa" <:> "Primordial Seal")
    [Ruins]
    TheHeartOfMadness

geothermalVent :: CardDef
geothermalVent =
  locationWithUnrevealed_
    "08650"
    "Ancient Facility"
    [Ruins]
    "Geothermal Vent"
    [Ruins]
    TheHeartOfMadness

forsakenTemple :: CardDef
forsakenTemple =
  locationWithUnrevealed_
    "08651"
    "Ancient Facility"
    [Ruins]
    "Forsaken Temple"
    [Ruins]
    TheHeartOfMadness

libraryOfKos :: CardDef
libraryOfKos =
  locationWithUnrevealed_
    "08652"
    "Ancient Facility"
    [Ruins]
    "Library of Kós"
    [Ruins]
    TheHeartOfMadness

protoplasmicPool :: CardDef
protoplasmicPool =
  locationWithUnrevealed_
    "08653"
    "Ancient Facility"
    [Ruins]
    "Protoplasmic Pool"
    [Ruins]
    TheHeartOfMadness

hallOfTheSunlessSea :: CardDef
hallOfTheSunlessSea =
  locationWithUnrevealed_
    "08654"
    "Ancient Facility"
    [Ruins]
    "Hall of the Sunless Sea"
    [Ruins]
    TheHeartOfMadness

subnauticalSprawl :: CardDef
subnauticalSprawl =
  locationWithUnrevealed_
    "08655"
    "Ancient Facility"
    [Ruins]
    "Subnautical Sprawl"
    [Ruins]
    TheHeartOfMadness

vaultedCorridor :: CardDef
vaultedCorridor =
  locationWithUnrevealed_
    "08656"
    "Ancient Facility"
    [Ruins]
    "Vaulted Corridor"
    [Ruins]
    TheHeartOfMadness

glacialGrotto :: CardDef
glacialGrotto =
  locationWithUnrevealed_
    "08664"
    "Ancient Facility"
    [Ruins]
    "Glacial Grotto"
    [Ruins]
    TheGreatSeal

sculpturedCrypt :: CardDef
sculpturedCrypt =
  locationWithUnrevealed_
    "08665"
    "Ancient Facility"
    [Ruins]
    "Sculptured Crypt"
    [Ruins]
    TheGreatSeal

undercityAltar :: CardDef
undercityAltar =
  locationWithUnrevealed_
    "08666"
    "Ancient Facility"
    [Ruins]
    "Undercity Altar"
    [Ruins]
    TheGreatSeal

ichorLadenTunnels :: CardDef
ichorLadenTunnels =
  locationWithUnrevealed_
    "08667"
    "Ancient Facility"
    [Ruins]
    "Ichor-Laden Tunnels"
    [Ruins]
    TheGreatSeal

limestoneCaverns :: CardDef
limestoneCaverns =
  locationWithUnrevealed_
    "08668"
    "Ancient Facility"
    [Ruins]
    "Limestone Caverns"
    [Ruins]
    TheGreatSeal

mistPylon_174 :: CardDef
mistPylon_174 =
  locationWithUnrevealed_
    "08674"
    "Mist-Pylon"
    [Ruins]
    "Mist-Pylon"
    [Ruins]
    StirringInTheDeep

mistPylon_175 :: CardDef
mistPylon_175 =
  locationWithUnrevealed_
    "08675"
    "Mist-Pylon"
    [Ruins]
    "Mist-Pylon"
    [Ruins]
    StirringInTheDeep

mistPylon_176 :: CardDef
mistPylon_176 =
  locationWithUnrevealed_
    "08676"
    "Mist-Pylon"
    [Ruins]
    "Mist-Pylon"
    [Ruins]
    StirringInTheDeep

mistPylon_177 :: CardDef
mistPylon_177 =
  locationWithUnrevealed_
    "08677"
    "Mist-Pylon"
    [Ruins]
    "Mist-Pylon"
    [Ruins]
    StirringInTheDeep

mistPylon_178 :: CardDef
mistPylon_178 =
  locationWithUnrevealed_
    "08678"
    "Mist-Pylon"
    [Ruins]
    "Mist-Pylon"
    [Ruins]
    StirringInTheDeep

titanicRamp_182 :: CardDef
titanicRamp_182 =
  locationWithUnrevealed_
    "08682"
    "Titanic Ramp"
    [Ruins]
    "Titanic Ramp"
    [Ruins]
    StirringInTheDeep

titanicRamp_183 :: CardDef
titanicRamp_183 =
  locationWithUnrevealed_
    "08683"
    "Titanic Ramp"
    [Ruins]
    "Titanic Ramp"
    [Ruins]
    StirringInTheDeep

titanicRamp_184 :: CardDef
titanicRamp_184 =
  locationWithUnrevealed_
    "08684"
    "Titanic Ramp"
    [Ruins]
    "Titanic Ramp"
    [Ruins]
    StirringInTheDeep

titanicRamp_185 :: CardDef
titanicRamp_185 =
  locationWithUnrevealed_
    "08685"
    "Titanic Ramp"
    [Ruins]
    "Titanic Ramp"
    [Ruins]
    StirringInTheDeep

hiddenTunnelAWayOut :: CardDef
hiddenTunnelAWayOut =
  location_
    "08686"
    ("Hidden Tunnel" <:> "A Way Out")
    [Ruins]
    StirringInTheDeep
