{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheForgottenAge where

import Arkham.Location.CardDefs.Import

expeditionCamp :: CardDef
expeditionCamp =
  location "04050" "Expedition Camp" [Campsite, Jungle] Circle [Square, Diamond, Moon] TheUntamedWilds

ruinsOfEztli :: CardDef
ruinsOfEztli =
  victory 2
    $ singleSided
    $ location "04053" "Ruins of Eztli" [Ancient, Ruins] Hourglass [Triangle, Heart] TheUntamedWilds

entryway :: CardDef
entryway = location "04060" "Entryway" [Ancient, Ruins] Circle [Square, Star] TheDoomOfEztli

ancientHall :: CardDef
ancientHall =
  singleSided
    $ location "04063" "Ancient Hall" [Ancient, Ruins] Square [Circle, Star, Diamond] TheDoomOfEztli

grandChamber :: CardDef
grandChamber =
  victory 1
    $ singleSided
    $ location "04064" "Grand Chamber" [Ancient, Ruins] Star [Circle, Square, Triangle] TheDoomOfEztli

burialPit :: CardDef
burialPit =
  victory 1
    $ singleSided
    $ location "04065" "Burial Pit" [Ancient, Ruins] Triangle [Star, Diamond, Squiggle] TheDoomOfEztli

undergroundRuins :: CardDef
undergroundRuins =
  vengeance 1
    $ singleSided
    $ location
      "04066"
      "Underground Ruins"
      [Ancient, Ruins]
      Diamond
      [Square, Triangle, Squiggle]
      TheDoomOfEztli

secretPassage :: CardDef
secretPassage =
  victory 1
    $ singleSided
    $ location
      "04067"
      "Secret Passage"
      [Ancient, Ruins]
      Squiggle
      [Diamond, Triangle, Hourglass]
      TheDoomOfEztli

chamberOfTime :: CardDef
chamberOfTime =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "04068"
      "Chamber of Time"
      [Forgotten, Ruins]
      Hourglass
      [Squiggle]
      TheDoomOfEztli

pathOfThorns :: CardDef
pathOfThorns =
  singleSided
    $ location
      "04069"
      "Path of Thorns"
      [Jungle]
      Square
      [Circle, Diamond, Triangle, Squiggle]
      Rainforest

riverCanyon :: CardDef
riverCanyon =
  singleSided
    $ location
      "04070"
      "River Canyon"
      [Jungle]
      Diamond
      [Circle, Moon, Heart, Triangle, Square]
      Rainforest

ropeBridge :: CardDef
ropeBridge =
  singleSided
    $ location
      "04071"
      "Rope Bridge"
      [Jungle]
      Moon
      [Circle, Diamond, Heart, T]
      Rainforest

serpentsHaven :: CardDef
serpentsHaven =
  victory 1
    $ singleSided
    $ location
      "04072"
      "Serpent's Haven"
      [Jungle]
      Triangle
      [Squiggle, Square, Diamond, Hourglass]
      Rainforest

circuitousTrail :: CardDef
circuitousTrail =
  victory 1
    $ singleSided
    $ location
      "04073"
      "Circuitous Trail"
      [Jungle]
      Heart
      [Hourglass, Diamond, Moon, T]
      Rainforest

templeOfTheFang :: CardDef
templeOfTheFang =
  victory 2
    $ singleSided
    $ location
      "04074"
      "Temple of the Fang"
      [Ancient, Ruins]
      Squiggle
      [Square, Triangle, Equals]
      Rainforest

overgrownRuins :: CardDef
overgrownRuins =
  victory 2
    $ singleSided
    $ location
      "04075"
      "Overgrown Ruins"
      [Ancient, Ruins]
      T
      [Moon, Heart, Equals]
      Rainforest

eztliExhibit :: CardDef
eztliExhibit =
  victory 1
    $ otherSideIs "04117"
    $ location
      "04117b"
      "Eztli Exhibit"
      [Miskatonic, Exhibit]
      Plus
      [Diamond]
      ThreadsOfFate

velmasDiner :: CardDef
velmasDiner =
  location "04141" "Velma's Diner" [Arkham] NoSymbol [Moon] ThreadsOfFate

curiositieShoppe :: CardDef
curiositieShoppe =
  victory 1
    $ location "04142" "Curiositie Shoppe" [Arkham] NoSymbol [T] ThreadsOfFate

townHall :: CardDef
townHall =
  victory 1
    $ location "04143" "Town Hall" [Arkham] NoSymbol [Triangle] ThreadsOfFate

arkhamPoliceStation :: CardDef
arkhamPoliceStation =
  victory 1
    $ otherSideIs "04126"
    $ location
      "04126b"
      "Arkham Police Station"
      [Arkham]
      NoSymbol
      [Moon]
      ThreadsOfFate

trainTracks :: CardDef
trainTracks =
  otherSideIs "04128"
    $ location "04128b" "Train Tracks" [Arkham] NoSymbol [T] ThreadsOfFate

blackCave :: CardDef
blackCave =
  victory 1
    $ otherSideIs "04133"
    $ location
      "04133b"
      "Black Cave"
      [Cave]
      Hourglass
      [Circle]
      ThreadsOfFate

templeRuins :: CardDef
templeRuins =
  location
    "04168"
    "Temple Ruins"
    [MexicoCity, PresentDay]
    Circle
    [Diamond, Star]
    TheBoundaryBeyond

metropolitanCathedral :: CardDef
metropolitanCathedral =
  location
    "04169"
    "Metropolitan Cathedral"
    [MexicoCity, PresentDay]
    Square
    [Diamond]
    TheBoundaryBeyond

chapultepecPark :: CardDef
chapultepecPark =
  location
    "04170"
    "Chapultepec Park"
    [MexicoCity, PresentDay]
    Triangle
    [Star]
    TheBoundaryBeyond

zocalo :: CardDef
zocalo =
  location
    "04171"
    "Zócalo"
    [MexicoCity, PresentDay]
    Diamond
    [Heart, Square, Star, Circle]
    TheBoundaryBeyond

xochimilco :: CardDef
xochimilco =
  location
    "04172"
    "Xochimilco"
    [MexicoCity, PresentDay]
    Heart
    [Diamond, Star]
    TheBoundaryBeyond

coyoacan :: CardDef
coyoacan =
  location
    "04173"
    "Coyoacán"
    [MexicoCity, PresentDay]
    Star
    [Diamond, Triangle, Circle, Heart]
    TheBoundaryBeyond

temploMayor_174 :: CardDef
temploMayor_174 =
  singleSided
    $ location
      "04174"
      "Templo Mayor"
      [Ancient, Tenochtitlan]
      Circle
      [Square, Triangle]
      TheBoundaryBeyond

temploMayor_175 :: CardDef
temploMayor_175 =
  singleSided
    $ location
      "04175"
      "Templo Mayor"
      [Ancient, Tenochtitlan]
      Circle
      [Square, Triangle]
      TheBoundaryBeyond

templesOfTenochtitlan_176 :: CardDef
templesOfTenochtitlan_176 =
  singleSided
    $ location
      "04176"
      "Temples of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Square
      [Diamond, Circle]
      TheBoundaryBeyond

templesOfTenochtitlan_177 :: CardDef
templesOfTenochtitlan_177 =
  singleSided
    $ location
      "04177"
      "Temples of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Square
      [Diamond, Circle]
      TheBoundaryBeyond

chapultepecHill_178 :: CardDef
chapultepecHill_178 =
  singleSided
    $ location
      "04178"
      "Chapultepec Hill"
      [Ancient, Tenochtitlan]
      Triangle
      [Star, Circle]
      TheBoundaryBeyond

chapultepecHill_179 :: CardDef
chapultepecHill_179 =
  singleSided
    $ location
      "04179"
      "Chapultepec Hill"
      [Ancient, Tenochtitlan]
      Triangle
      [Star, Circle]
      TheBoundaryBeyond

canalsOfTenochtitlan_180 :: CardDef
canalsOfTenochtitlan_180 =
  singleSided
    $ location
      "04180"
      "Canals of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Diamond
      [Heart, Square]
      TheBoundaryBeyond

canalsOfTenochtitlan_181 :: CardDef
canalsOfTenochtitlan_181 =
  singleSided
    $ location
      "04181"
      "Canals of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Diamond
      [Heart, Square]
      TheBoundaryBeyond

lakeXochimilco_182 :: CardDef
lakeXochimilco_182 =
  singleSided
    $ location
      "04182"
      "Lake Xochimilco"
      [Ancient, Tenochtitlan]
      Heart
      [Diamond, Star]
      TheBoundaryBeyond

lakeXochimilco_183 :: CardDef
lakeXochimilco_183 =
  singleSided
    $ location
      "04183"
      "Lake Xochimilco"
      [Ancient, Tenochtitlan]
      Heart
      [Diamond, Star]
      TheBoundaryBeyond

sacredWoods_184 :: CardDef
sacredWoods_184 =
  singleSided
    $ location
      "04184"
      "Sacred Woods"
      [Ancient, Tenochtitlan]
      Star
      [Heart, Triangle]
      TheBoundaryBeyond

sacredWoods_185 :: CardDef
sacredWoods_185 =
  singleSided
    $ location
      "04185"
      "Sacred Woods"
      [Ancient, Tenochtitlan]
      Star
      [Heart, Triangle]
      TheBoundaryBeyond

mouthOfKnYanTheCavernsMaw :: CardDef
mouthOfKnYanTheCavernsMaw =
  otherSideIs "04206b"
    $ location
      "04206"
      ("Mouth of K'n-yan" <:> "The Cavern's Maw")
      [Cave]
      Equals
      [Squiggle, T, Hourglass]
      HeartOfTheElders

mouthOfKnYanTheDepthsBeneath :: CardDef
mouthOfKnYanTheDepthsBeneath =
  otherSideIs "04206"
    $ location
      "04206b"
      ("Mouth of K'n-yan" <:> "The Depths Beneath")
      [Cave]
      Equals
      [Circle, Triangle, Diamond]
      HeartOfTheElders

timeWrackedWoods :: CardDef
timeWrackedWoods =
  victory 1
    $ singleSided
    $ location
      "04217"
      "Time-Wracked Woods"
      [Jungle]
      Circle
      [Square, Diamond, Moon]
      PillarsOfJudgement

stoneAltar :: CardDef
stoneAltar =
  victory 1
    $ singleSided
    $ location
      "04218"
      "Stone Altar"
      [Ancient, Ruins]
      Hourglass
      [Triangle, Heart, Equals]
      PillarsOfJudgement

vastPassages :: CardDef
vastPassages =
  singleSided
    $ location
      "04222"
      "Vast Passages"
      [Ancient, Cave]
      Circle
      [Equals, Triangle, Diamond, Square, Moon]
      KnYan

hallOfIdolatry :: CardDef
hallOfIdolatry =
  victory 1
    $ singleSided
    $ location
      "04223"
      "Hall of Idolatry"
      [Ancient, Cave]
      Square
      [Heart, Triangle, Circle]
      KnYan

darkHollow :: CardDef
darkHollow =
  victory 1
    $ singleSided
    $ location
      "04224"
      "Dark Hollow"
      [Ancient, Cave]
      Triangle
      [Equals, Circle, Square]
      KnYan

perilousGulch :: CardDef
perilousGulch =
  victory 1
    $ singleSided
    $ location
      "04225"
      "Perilous Gulch"
      [Ancient, Cave]
      Diamond
      [Equals, Circle, Moon]
      KnYan

crystalPillars :: CardDef
crystalPillars =
  victory 1
    $ singleSided
    $ location
      "04226"
      "Crystal Pillars"
      [Ancient, Cave]
      Moon
      [Heart, Diamond, Circle]
      KnYan

descentToYoth :: CardDef
descentToYoth =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "04227"
      "Descent to Yoth"
      [Ancient, Cave]
      Heart
      [Square, Moon]
      KnYan

interviewRoomArrivalChamber :: CardDef
interviewRoomArrivalChamber =
  locationWithUnrevealed
    "04245"
    "Interview Room"
    [Ancient, Pnakotus]
    Droplet
    [Diamond]
    ("Interview Room" <:> "Arrival Chamber")
    [Ancient, Pnakotus]
    Droplet
    [Diamond]
    TheCityOfArchives

interviewRoomRestrainingChamber :: CardDef
interviewRoomRestrainingChamber =
  victory 1
    $ locationWithUnrevealed
      "04246"
      "Interview Room"
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      ("Interview Room" <:> "Restraining Chamber")
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      TheCityOfArchives

interviewRoomIchorFilledChamber :: CardDef
interviewRoomIchorFilledChamber =
  victory 1
    $ locationWithUnrevealed
      "04247"
      "Interview Room"
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      ("Interview Room" <:> "Ichor-Filled Chamber")
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      TheCityOfArchives

hallsOfPnakotusNorthernCorridors :: CardDef
hallsOfPnakotusNorthernCorridors =
  location
    "04248"
    ("Halls of Pnakotus" <:> "Northern Corridors")
    [Ancient, Pnakotus]
    Squiggle
    [Diamond, Square, Triangle]
    TheCityOfArchives

hallsOfPnakotusEasternCorridors :: CardDef
hallsOfPnakotusEasternCorridors =
  location
    "04249"
    ("Halls of Pnakotus" <:> "Eastern Corridors")
    [Ancient, Pnakotus]
    Diamond
    [Squiggle, Square, Droplet]
    TheCityOfArchives

hallsOfPnakotusWesternCorridors :: CardDef
hallsOfPnakotusWesternCorridors =
  location
    "04250"
    ("Halls of Pnakotus" <:> "Western Corridors")
    [Ancient, Pnakotus]
    Square
    [Squiggle, Diamond, Circle, Star]
    TheCityOfArchives

greatLibrary :: CardDef
greatLibrary =
  location
    "04251"
    "Great Library"
    [Ancient, Pnakotus]
    Circle
    [Square]
    TheCityOfArchives

yithianOrrery :: CardDef
yithianOrrery =
  victory 1
    $ location
      "04252"
      "Yithian Orrery"
      [Ancient, Pnakotus]
      Moon
      [Triangle]
      TheCityOfArchives

laboratoryOfTheGreatRace :: CardDef
laboratoryOfTheGreatRace =
  location
    "04253"
    "Laboratory of the Great Race"
    [Ancient, Pnakotus]
    Triangle
    [Squiggle, Moon, Equals]
    TheCityOfArchives

deconstructionRoom :: CardDef
deconstructionRoom =
  victory 1
    $ location
      "04254"
      "Deconstruction Room"
      [Ancient, Pnakotus]
      Equals
      [Triangle]
      TheCityOfArchives

towersOfPnakotus :: CardDef
towersOfPnakotus =
  victory 2
    $ location
      "04255"
      "Towers of Pnakotus"
      [Ancient, Pnakotus]
      Star
      [Square]
      TheCityOfArchives

stepsOfYoth :: CardDef
stepsOfYoth =
  singleSided
    $ location
      "04286"
      "Steps of Yoth"
      [Ancient, Forgotten, Yoth]
      Equals
      [Hourglass, Square, Triangle, Diamond, Heart]
      TheDepthsOfYoth

cityOfTheSerpents :: CardDef
cityOfTheSerpents =
  vengeance 2
    $ singleSided
    $ location
      "04287"
      "City of the Serpents"
      [Ancient, Cave, Yoth]
      Diamond
      [Equals, Droplet, Triangle, T, Square]
      TheDepthsOfYoth

hallOfHeresy :: CardDef
hallOfHeresy =
  vengeance 2
    $ singleSided
    $ location
      "04288"
      "Hall of Heresy"
      [Ancient, Cave, Yoth]
      Triangle
      [Equals, Diamond, Circle, Square, T]
      TheDepthsOfYoth

crumblingPrecipice :: CardDef
crumblingPrecipice =
  singleSided
    $ location
      "04289"
      "Crumbling Precipice"
      [Ancient, Cave, Yoth]
      Hourglass
      [Equals, Squiggle, Heart, T, Droplet]
      TheDepthsOfYoth

cavernsOfYoth :: CardDef
cavernsOfYoth =
  singleSided
    $ location
      "04290"
      "Caverns of Yoth"
      [Ancient, Cave, Yoth]
      Droplet
      [Circle, Hourglass, Heart, Diamond, Squiggle]
      TheDepthsOfYoth

forkedPath :: CardDef
forkedPath =
  singleSided
    $ location
      "04291"
      "Forked Path"
      [Ancient, Cave, Yoth]
      T
      [Circle, Diamond, Hourglass, Square, Triangle]
      TheDepthsOfYoth

bridgeOverNKai :: CardDef
bridgeOverNKai =
  singleSided
    $ location
      "04292"
      "Bridge over N'kai"
      [Ancient, Cave, Yoth]
      Heart
      [Equals, Circle, Droplet, Hourglass, Squiggle]
      TheDepthsOfYoth

brokenPassage :: CardDef
brokenPassage =
  singleSided
    $ location
      "04293"
      "Broken Passage"
      [Ancient, Cave, Yoth]
      Squiggle
      [Circle, Droplet, Hourglass, Square, Heart]
      TheDepthsOfYoth

abandonedSite :: CardDef
abandonedSite =
  singleSided
    $ location
      "04294"
      "Abandoned Site"
      [Ancient, Cave, Yoth]
      Square
      [Equals, Diamond, Triangle, T, Squiggle]
      TheDepthsOfYoth

brightCanyon :: CardDef
brightCanyon =
  singleSided
    $ location
      "04295"
      "Bright Canyon"
      [Ancient, Cave, Yoth]
      Circle
      [Droplet, Squiggle, T, Heart, Triangle]
      TheDepthsOfYoth

nexusOfNKai :: CardDef
nexusOfNKai =
  location
    "04324"
    ("Nexus of N'kai" <:> "Unraveling the Threads")
    [Ancient, Ruins]
    Diamond
    [Droplet, Star]
    ShatteredAeons

yuggoth :: CardDef
yuggoth =
  singleSided
    $ location "04327" "Yuggoth" [Otherworld] Droplet [Diamond] ShatteredAeons

shoresOfRlyeh :: CardDef
shoresOfRlyeh =
  singleSided
    $ location
      "04328"
      "Shores of R'lyeh"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

cityOfTheUnseen :: CardDef
cityOfTheUnseen =
  singleSided
    $ location
      "04329"
      "City of the Unseen"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

aPocketInTime :: CardDef
aPocketInTime =
  victory 1
    $ singleSided
    $ location
      "04330"
      "A Pocket in Time"
      [Extradimensional]
      Star
      [Diamond, Equals]
      ShatteredAeons

ruinsOfNewYork :: CardDef
ruinsOfNewYork =
  singleSided
    $ location
      "04331"
      "Ruins of New York"
      [Shattered, Future, Ruins]
      Equals
      [Star]
      ShatteredAeons

mu :: CardDef
mu =
  victory 1
    $ singleSided
    $ location
      "04332"
      "Mu"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

atlantis :: CardDef
atlantis =
  singleSided
    $ location
      "04333"
      "Atlantis"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

pnakotus :: CardDef
pnakotus =
  victory 1
    $ singleSided
    $ location
      "04334"
      "Pnakotus"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

valusia :: CardDef
valusia =
  victory 1
    $ singleSided
    $ location
      "04335"
      "Valusia"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

plateauOfLeng :: CardDef
plateauOfLeng =
  singleSided
    $ location
      "04336"
      "Plateau of Leng"
      [Shattered, PresentDay]
      Equals
      [Star]
      ShatteredAeons
