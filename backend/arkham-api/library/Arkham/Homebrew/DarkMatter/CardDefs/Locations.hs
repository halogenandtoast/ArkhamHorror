module Arkham.Homebrew.DarkMatter.CardDefs.Locations where

import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits hiding (pattern Moon)
import Arkham.Homebrew.DarkMatter.Traits qualified as Trait
import Arkham.Location.CardDefs.Import

scanIcons :: [LocationSymbol] -> CardDef -> CardDef
scanIcons icons = singleSidedWithFlippedBack . withMeta ("scanIcons", icons)

-- the_tatterdemalion
cargoHold :: CardDef
cargoHold =
  location
    ":dark-matter:022"
    "Cargo Hold"
    [Tatterdemalion, Access]
    Square
    [Circle, Hourglass, Moon]
    Set.TheTatterdemalion

cryosleepQuarters :: CardDef
cryosleepQuarters =
  location
    ":dark-matter:023"
    "Cryosleep Quarters"
    [Tatterdemalion]
    Triangle
    [Circle]
    Set.TheTatterdemalion

engineRoomTatterdemalion :: CardDef
engineRoomTatterdemalion =
  location
    ":dark-matter:024"
    "Engine Room"
    [Tatterdemalion]
    Hourglass
    [Square]
    Set.TheTatterdemalion

escapePodBay :: CardDef
escapePodBay =
  location
    ":dark-matter:025"
    "Escape Pod Bay"
    [Tatterdemalion, Access]
    Moon
    [Square]
    Set.TheTatterdemalion

infirmaryTatterdemalion :: CardDef
infirmaryTatterdemalion =
  location
    ":dark-matter:026"
    "Infirmary"
    [Tatterdemalion]
    Plus
    [Circle, Equals]
    Set.TheTatterdemalion

messHall :: CardDef
messHall =
  location
    ":dark-matter:027"
    "Mess Hall"
    [Tatterdemalion]
    Circle
    [Square, T, Triangle, Plus, Equals]
    Set.TheTatterdemalion

shipsBridge :: CardDef
shipsBridge =
  location
    ":dark-matter:028"
    "Ship's Bridge"
    [Tatterdemalion, Access]
    T
    [Circle, Equals]
    Set.TheTatterdemalion

ventilationShaft :: CardDef
ventilationShaft =
  scanIcons [Circle, Plus]
    $ location
      ":dark-matter:041"
      "Ventilation Shaft"
      [Tatterdemalion]
      Equals
      [Circle, Plus, T]
      Set.TheTatterdemalion

-- electric_nightmare
cafeteria :: CardDef
cafeteria =
  locationWithUnrevealedName
    ":dark-matter:069"
    "Undefined Room"
    "Cafeteria"
    [School]
    Square
    []
    Set.ElectricNightmare

classroomK2 :: CardDef
classroomK2 =
  victory 1
    $ locationWithUnrevealedName
      ":dark-matter:070"
      "Undefined Room"
      "Classroom K2"
      [School]
      Circle
      []
      Set.ElectricNightmare

entranceHall :: CardDef
entranceHall =
  locationWithUnrevealedName
    ":dark-matter:071"
    "A Shimmer in the Wall"
    "Entrance Hall"
    [School]
    Squiggle
    []
    Set.ElectricNightmare

gymnasium :: CardDef
gymnasium =
  locationWithUnrevealedName
    ":dark-matter:072"
    "Undefined Room"
    "Gymnasium"
    [School]
    Triangle
    []
    Set.ElectricNightmare

biologyLab :: CardDef
biologyLab =
  locationWithUnrevealedName
    ":dark-matter:073"
    "Undefined Room"
    "Biology Lab"
    [School]
    Moon
    []
    Set.ElectricNightmare

library :: CardDef
library =
  victory 1
    $ locationWithUnrevealedName
      ":dark-matter:074"
      "Undefined Room"
      "Library"
      [School]
      Plus
      []
      Set.ElectricNightmare

schoolGrounds :: CardDef
schoolGrounds =
  victory 1 $ location ":dark-matter:075" "School Grounds" [] Diamond [] Set.ElectricNightmare

-- lost_quantum
coldWastes :: CardDef
coldWastes =
  scanIcons [Circle, Trefoil, Moon]
    $ location ":dark-matter:096" "Cold Wastes" [Elbrus] Hourglass [Circle, Trefoil, Moon] Set.LostQuantum

crystalPeak :: CardDef
crystalPeak =
  scanIcons [Diamond, Equals, Hourglass]
    $ victory 2
    $ location
      ":dark-matter:097"
      "Crystal Peak"
      [Elbrus, Extradimensional]
      Moon
      [Diamond, Equals, Hourglass]
      Set.LostQuantum

iceSpires :: CardDef
iceSpires =
  scanIcons [Triangle, Moon, Diamond]
    $ victory 1
    $ location ":dark-matter:098" "Ice Spires" [Elbrus] Equals [Triangle, Moon, Diamond] Set.LostQuantum

landingCraft :: CardDef
landingCraft =
  scanIcons [Hourglass, Square, Triangle]
    $ location
      ":dark-matter:099"
      "Landing Craft"
      [Elbrus]
      Circle
      [Hourglass, Square, Triangle]
      Set.LostQuantum

mainFacility :: CardDef
mainFacility =
  scanIcons [Circle, Trefoil, Diamond]
    $ victory 1
    $ location
      ":dark-matter:100"
      "Main Facility"
      [Elbrus]
      Square
      [Circle, Trefoil, Diamond]
      Set.LostQuantum

omniTransmitters :: CardDef
omniTransmitters =
  scanIcons [Circle, Equals, Trefoil]
    $ location
      ":dark-matter:101"
      "Omni-Transmitters"
      [Elbrus]
      Triangle
      [Circle, Equals, Trefoil]
      Set.LostQuantum

qCrystalMines :: CardDef
qCrystalMines =
  scanIcons [Square, Moon, Equals]
    $ victory 1
    $ location
      ":dark-matter:102"
      "Q-Crystal Mines"
      [Elbrus]
      Diamond
      [Square, Moon, Equals]
      Set.LostQuantum

schrodGenerators :: CardDef
schrodGenerators =
  scanIcons [Square, Triangle, Hourglass]
    $ location
      ":dark-matter:103"
      "Schröd Generators"
      [Elbrus]
      Trefoil
      [Square, Triangle, Hourglass]
      Set.LostQuantum

-- in_the_shadow_of_earth
airlocks :: CardDef
airlocks =
  location
    ":dark-matter:122"
    "Airlocks"
    [NostalgiaII]
    Circle
    [Triangle, Diamond]
    Set.InTheShadowOfEarth

crewQuarters :: CardDef
crewQuarters =
  location
    ":dark-matter:123"
    "Crew Quarters"
    [NostalgiaII]
    Hourglass
    [Square, Triangle, Diamond, Equals]
    Set.InTheShadowOfEarth

engineRoomInTheShadowOfEarth :: CardDef
engineRoomInTheShadowOfEarth =
  victory 1
    $ location
      ":dark-matter:124"
      "Engine Room"
      [NostalgiaII]
      Square
      [Triangle, Hourglass]
      Set.InTheShadowOfEarth

flightDeck :: CardDef
flightDeck =
  victory 1
    $ location ":dark-matter:125" "Flight Deck" [NostalgiaII] T [Trefoil, Equals] Set.InTheShadowOfEarth

hydroponics :: CardDef
hydroponics =
  location
    ":dark-matter:126"
    "Hydroponics"
    [NostalgiaII]
    Diamond
    [Circle, Triangle, Hourglass, Trefoil]
    Set.InTheShadowOfEarth

infirmaryInTheShadowOfEarth :: CardDef
infirmaryInTheShadowOfEarth =
  location
    ":dark-matter:127"
    "Infirmary"
    [NostalgiaII]
    Trefoil
    [Diamond, Equals, T]
    Set.InTheShadowOfEarth

shipMainframe :: CardDef
shipMainframe =
  location
    ":dark-matter:128"
    "Ship Mainframe"
    [NostalgiaII]
    Triangle
    [Circle, Square, Diamond, Hourglass]
    Set.InTheShadowOfEarth

telecoms :: CardDef
telecoms =
  location
    ":dark-matter:129"
    "Telecoms"
    [NostalgiaII]
    Equals
    [Hourglass, Trefoil, T]
    Set.InTheShadowOfEarth

-- strange_moons
brainStorage :: CardDef
brainStorage =
  locationWithUnrevealedName
    ":dark-matter:164"
    "Alien Chambers"
    "Brain Storage"
    [Interface]
    Moon
    [Triangle, Circle, Trefoil, Hourglass, Squiggle]
    Set.StrangeMoons

communicator :: CardDef
communicator =
  locationWithUnrevealedName
    ":dark-matter:165"
    "Alien Chambers"
    "Communicator"
    [Interface]
    Hourglass
    [Moon]
    Set.StrangeMoons

dreamDiagnostics :: CardDef
dreamDiagnostics =
  locationWithUnrevealedName
    ":dark-matter:166"
    "Alien Chambers"
    "Dream Diagnostics"
    [Interface]
    Trefoil
    [Moon, Circle]
    Set.StrangeMoons

entranceTunnel :: CardDef
entranceTunnel = location ":dark-matter:167" "Entrance Tunnel" [] Squiggle [Moon] Set.StrangeMoons

memoryScanner :: CardDef
memoryScanner =
  locationWithUnrevealedName
    ":dark-matter:168"
    "Alien Chambers"
    "Memory Scanner"
    [Interface]
    Triangle
    [Moon, Circle]
    Set.StrangeMoons

realitySimulator :: CardDef
realitySimulator =
  locationWithUnrevealedName
    ":dark-matter:169"
    "Alien Chambers"
    "Reality Simulator"
    []
    Circle
    [Moon, Trefoil, Triangle]
    Set.StrangeMoons

aHidingPlace :: CardDef
aHidingPlace =
  scanIcons [Triangle, Square]
    $ victory 1
    $ location
      ":dark-matter:170"
      "A Hiding Place"
      [Simulation]
      Circle
      [Moon, Trefoil, Triangle]
      Set.StrangeMoons

aMutiny :: CardDef
aMutiny =
  scanIcons [Triangle, Diamond]
    $ location
      ":dark-matter:171"
      "A Mutiny"
      [Memory, Simulation]
      Circle
      [Moon, Trefoil, Triangle]
      Set.StrangeMoons

adriftInSpace :: CardDef
adriftInSpace =
  scanIcons [Trefoil, Equals]
    $ location
      ":dark-matter:172"
      "Adrift in Space"
      [Simulation, Nightmare]
      Circle
      [Moon, Trefoil, Triangle]
      Set.StrangeMoons

anAccident :: CardDef
anAccident =
  scanIcons [Triangle, Equals]
    $ victory 1
    $ location
      ":dark-matter:173"
      "An Accident"
      [Memory, Simulation]
      Circle
      [Moon, Trefoil, Triangle]
      Set.StrangeMoons

cityOfCats :: CardDef
cityOfCats =
  scanIcons [Trefoil, Square]
    $ victory 1
    $ location
      ":dark-matter:174"
      "City of Cats"
      [Simulation, Dreamlands]
      Circle
      [Moon, Trefoil, Triangle]
      Set.StrangeMoons

feverDream :: CardDef
feverDream =
  scanIcons [Trefoil, Diamond]
    $ victory 1
    $ location
      ":dark-matter:175"
      "Fever Dream"
      [Simulation, Nightmare]
      Circle
      [Moon, Trefoil, Triangle]
      Set.StrangeMoons

-- fragment_of_carcosa
abandonedLander :: CardDef
abandonedLander =
  location
    ":dark-matter:216"
    "Abandoned Lander"
    [Surface]
    Square
    [Circle, Triangle]
    Set.FragmentOfCarcosa

surfaceOfFragment :: CardDef
surfaceOfFragment =
  location
    ":dark-matter:217"
    "Surface of Fragment"
    [Surface]
    Circle
    [Square, Triangle]
    Set.FragmentOfCarcosa

bottomlessPit :: CardDef
bottomlessPit =
  otherSideIs ":dark-matter:218b"
    $ location
      ":dark-matter:218"
      "Bottomless Pit"
      [Cave]
      Plus
      [Triangle, Trefoil, Diamond, Hourglass]
      Set.FragmentOfCarcosa

spiralStaircase :: CardDef
spiralStaircase =
  otherSideIs ":dark-matter:218"
    $ location
      ":dark-matter:218b"
      "Spiral Staircase"
      [Carcosa]
      Plus
      [Triangle, Trefoil, Diamond, Hourglass]
      Set.FragmentOfCarcosa

cyclopeanCaverns :: CardDef
cyclopeanCaverns =
  otherSideIs ":dark-matter:219b"
    $ location
      ":dark-matter:219"
      "Cyclopean Caverns"
      [Cave]
      Diamond
      [Plus, Trefoil, T, Hourglass]
      Set.FragmentOfCarcosa

grandBallroom :: CardDef
grandBallroom =
  otherSideIs ":dark-matter:219"
    $ location
      ":dark-matter:219b"
      "Grand Ballroom"
      [Carcosa]
      Diamond
      [Plus, Trefoil, T, Hourglass]
      Set.FragmentOfCarcosa

hiddenPassage :: CardDef
hiddenPassage =
  otherSideIs ":dark-matter:220b"
    $ location
      ":dark-matter:220"
      "Hidden Passage"
      [Surface, Cave]
      Triangle
      [Circle, Square, Plus]
      Set.FragmentOfCarcosa

palaceGates :: CardDef
palaceGates =
  otherSideIs ":dark-matter:220"
    $ location
      ":dark-matter:220b"
      "Palace Gates"
      [Surface, Carcosa]
      Triangle
      [Circle, Square, Plus]
      Set.FragmentOfCarcosa

iceCavity :: CardDef
iceCavity =
  otherSideIs ":dark-matter:221b"
    $ location ":dark-matter:221" "Ice Cavity" [Cave] Trefoil [Plus, Diamond, T] Set.FragmentOfCarcosa

gardensOfThothut :: CardDef
gardensOfThothut =
  otherSideIs ":dark-matter:221"
    $ location
      ":dark-matter:221b"
      "Gardens of Thothut"
      [Carcosa]
      Trefoil
      [Plus, Diamond, T]
      Set.FragmentOfCarcosa

impassableRavine :: CardDef
impassableRavine =
  otherSideIs ":dark-matter:222b"
    $ victory 1
    $ location
      ":dark-matter:222"
      ("Impassable Ravine" <:> "Where the Cultists Disappeared")
      [Cave]
      T
      [Trefoil, Diamond, Hourglass]
      Set.FragmentOfCarcosa

theYellowThrone :: CardDef
theYellowThrone =
  otherSideIs ":dark-matter:222"
    $ victory 1
    $ location
      ":dark-matter:222b"
      ("The Yellow Throne" <:> "Where Tassilda Reigns")
      [Carcosa]
      T
      [Trefoil, Diamond, Hourglass]
      Set.FragmentOfCarcosa

stalagmiteForest :: CardDef
stalagmiteForest =
  otherSideIs ":dark-matter:223b"
    $ location
      ":dark-matter:223"
      "Stalagmite Forest"
      [Cave]
      Hourglass
      [Plus, Diamond, T]
      Set.FragmentOfCarcosa

labyrinthsOfTasylock :: CardDef
labyrinthsOfTasylock =
  otherSideIs ":dark-matter:223"
    $ location
      ":dark-matter:223b"
      "Labyrinths of Tasylock"
      [Carcosa]
      Hourglass
      [Plus, Diamond, T]
      Set.FragmentOfCarcosa

-- starfall

sol :: CardDef
sol =
  otherSideIs ":dark-matter:244" $ location ":dark-matter:244b" "Sol" [Sol] Plus [] Set.Starfall

theTatterdemalion :: CardDef
theTatterdemalion =
  location ":dark-matter:250" "The Tatterdemalion" [Starship] T [] Set.Starfall

newBrooklyn :: CardDef
newBrooklyn =
  locationWithUnrevealedName
    ":dark-matter:251"
    ("Asteroid Belt" <:> "A Sight For Sore Eyes")
    ("New Brooklyn" <:> "Population: 4034 Humans")
    [Colony, AsteroidBelt]
    Heart
    [Diamond]
    Set.Starfall

hope :: CardDef
hope =
  locationWithUnrevealedName
    ":dark-matter:252"
    ("Mars" <:> "A Glimmer of Hope")
    ("Hope" <:> "Population: 138021 Humans")
    [Colony, Mars]
    Circle
    [Triangle, Square]
    Set.Starfall

yuggoth :: CardDef
yuggoth =
  victory 1
    $ locationWithUnrevealedName
      ":dark-matter:253"
      ("Pluto..?" <:> "Mysterious Signals")
      ("Yuggoth" <:> "Population: 23615 Mi-Go")
      [Colony, Pluto]
      Moon
      [Trefoil]
      Set.Starfall

theCassilda :: CardDef
theCassilda = location ":dark-matter:255" "The Cassilda" [Starship] T [] Set.Starfall

earth :: CardDef
earth =
  scanIcons [T, Triangle]
    $ victory 2
    $ location ":dark-matter:259" ("Earth" <:> "The Stars Were Right") [Earth] Droplet [] Set.Starfall

mountSinai :: CardDef
mountSinai =
  scanIcons [Heart]
    $ victory 1
    $ location ":dark-matter:260" "Mount Sinai" [AsteroidBelt] Diamond [Heart] Set.Starfall

derelictShip :: CardDef
derelictShip =
  scanIcons [T, Triangle]
    $ location ":dark-matter:261" "Derelict Ship" [Starship] Hourglass [] Set.Starfall

martianRuins :: CardDef
martianRuins =
  scanIcons [Circle, Triangle]
    $ victory 1
    $ location ":dark-matter:262" "Martian Ruins" [Mars] Square [Circle, Triangle] Set.Starfall

olympusTelescope :: CardDef
olympusTelescope =
  scanIcons [Circle, Square]
    $ location ":dark-matter:263" "Olympus Telescope" [Mars] Triangle [Circle, Square] Set.Starfall

moonbaseLaboratory :: CardDef
moonbaseLaboratory =
  scanIcons [T, Triangle]
    $ victory 1
    $ location ":dark-matter:264" "Moonbase Laboratory" [Colony, Trait.Moon] Equals [] Set.Starfall

thresholdOfYuggoth :: CardDef
thresholdOfYuggoth =
  scanIcons [Moon]
    $ victory 1
    $ location ":dark-matter:265" "Threshold of Yuggoth" [Pluto] Trefoil [Moon] Set.Starfall
