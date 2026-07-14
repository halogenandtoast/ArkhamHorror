module Arkham.Homebrew.DarkMatter.CardDefs.Locations where

import Arkham.Location.CardDefs.Import
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.Traits hiding (pattern Moon)
import Arkham.Homebrew.Traits qualified as Trait

-- the_tatterdemalion
cargoHold :: CardDef
cargoHold =
  location
    ":dark-matter:021"
    "Cargo Hold"
    [Tatterdemalion, Access]
    Square
    [Circle, Hourglass, Moon]
    Set.TheTatterdemalion

cryosleepQuarters :: CardDef
cryosleepQuarters =
  location
    ":dark-matter:022"
    "Cryosleep Quarters"
    [Tatterdemalion]
    Triangle
    [Circle]
    Set.TheTatterdemalion

engineRoom_023 :: CardDef
engineRoom_023 =
  location
    ":dark-matter:023"
    "Engine Room"
    [Tatterdemalion]
    Hourglass
    [Square]
    Set.TheTatterdemalion

escapePodBay :: CardDef
escapePodBay =
  location
    ":dark-matter:024"
    "Escape Pod Bay"
    [Tatterdemalion, Access]
    Moon
    [Square]
    Set.TheTatterdemalion

infirmary_025 :: CardDef
infirmary_025 =
  location
    ":dark-matter:025"
    "Infirmary"
    [Tatterdemalion]
    Plus
    [Circle, Equals]
    Set.TheTatterdemalion

messHall :: CardDef
messHall =
  location
    ":dark-matter:026"
    "Mess Hall"
    [Tatterdemalion]
    Circle
    [Square, T, Triangle, Plus, Equals]
    Set.TheTatterdemalion

shipsBridge :: CardDef
shipsBridge =
  location
    ":dark-matter:027"
    "Ship's Bridge"
    [Tatterdemalion, Access]
    T
    [Circle, Equals]
    Set.TheTatterdemalion

ventilationShaft :: CardDef
ventilationShaft =
  withMeta ("scanIcons", [Circle, Plus])
    $ location
      ":dark-matter:040"
      "Ventilation Shaft"
      [Tatterdemalion]
      Equals
      [Circle, Plus, T]
      Set.TheTatterdemalion

-- electric_nightmare
cafeteria :: CardDef
cafeteria = location_ ":dark-matter:071" "Cafeteria" [School] Set.ElectricNightmare

classroomK2 :: CardDef
classroomK2 =
  victory 1 $ location_ ":dark-matter:072" "Classroom K2" [School] Set.ElectricNightmare

entranceHall :: CardDef
entranceHall =
  location_ ":dark-matter:073" "Entrance Hall" [School] Set.ElectricNightmare

gymnasium :: CardDef
gymnasium = location_ ":dark-matter:074" "Gymnasium" [School] Set.ElectricNightmare

biologyLab :: CardDef
biologyLab =
  location_ ":dark-matter:075" "Biology Lab" [School] Set.ElectricNightmare

library :: CardDef
library =
  victory 1 $ location_ ":dark-matter:076" "Library" [School] Set.ElectricNightmare

schoolGrounds :: CardDef
schoolGrounds =
  victory 1 $ location_ ":dark-matter:077" "School Grounds" [] Set.ElectricNightmare

-- lost_quantum
coldWastes :: CardDef
coldWastes = location_ ":dark-matter:099" "Cold Wastes" [Elbrus] Set.LostQuantum

crystalPeak :: CardDef
crystalPeak =
  victory 2
    $ location_ ":dark-matter:100" "Crystal Peak" [Elbrus, Extradimensional] Set.LostQuantum

iceSpires :: CardDef
iceSpires =
  victory 1 $ location_ ":dark-matter:101" "Ice Spires" [Elbrus] Set.LostQuantum

landingCraft :: CardDef
landingCraft =
  location_ ":dark-matter:102" "Landing Craft" [Elbrus] Set.LostQuantum

mainFacility :: CardDef
mainFacility =
  victory 1 $ location_ ":dark-matter:103" "Main Facility" [Elbrus] Set.LostQuantum

omniTransmitters :: CardDef
omniTransmitters =
  location_ ":dark-matter:104" "Omni-Transmitters" [Elbrus] Set.LostQuantum

qCrystalMines :: CardDef
qCrystalMines =
  victory 1 $ location_ ":dark-matter:105" "Q-Crystal Mines" [Elbrus] Set.LostQuantum

schrodGenerators :: CardDef
schrodGenerators =
  location_ ":dark-matter:106" "Schröd Generators" [Elbrus] Set.LostQuantum

-- in_the_shadow_of_earth
airlocks :: CardDef
airlocks =
  location_ ":dark-matter:125" "Airlocks" [NostalgiaII] Set.InTheShadowOfEarth

crewQuarters :: CardDef
crewQuarters =
  location_ ":dark-matter:126" "Crew Quarters" [NostalgiaII] Set.InTheShadowOfEarth

engineRoom_127 :: CardDef
engineRoom_127 =
  victory 1 $ location_ ":dark-matter:127" "Engine Room" [NostalgiaII] Set.InTheShadowOfEarth

flightDeck :: CardDef
flightDeck =
  victory 1 $ location_ ":dark-matter:128" "Flight Deck" [NostalgiaII] Set.InTheShadowOfEarth

hydroponics :: CardDef
hydroponics =
  location_ ":dark-matter:129" "Hydroponics" [NostalgiaII] Set.InTheShadowOfEarth

infirmary_130 :: CardDef
infirmary_130 =
  location_ ":dark-matter:130" "Infirmary" [NostalgiaII] Set.InTheShadowOfEarth

shipMainframe :: CardDef
shipMainframe =
  location_ ":dark-matter:131" "Ship Mainframe" [NostalgiaII] Set.InTheShadowOfEarth

telecoms :: CardDef
telecoms =
  location_ ":dark-matter:132" "Telecoms" [NostalgiaII] Set.InTheShadowOfEarth

-- strange_moons
brainStorage :: CardDef
brainStorage =
  location_ ":dark-matter:167" "Brain Storage" [Interface] Set.StrangeMoons

communicator :: CardDef
communicator =
  location_ ":dark-matter:168" "Communicator" [Interface] Set.StrangeMoons

dreamDiagnostics :: CardDef
dreamDiagnostics =
  location_ ":dark-matter:169" "Dream Diagnostics" [Interface] Set.StrangeMoons

entranceTunnel :: CardDef
entranceTunnel = location_ ":dark-matter:170" "Entrance Tunnel" [] Set.StrangeMoons

memoryScanner :: CardDef
memoryScanner =
  location_ ":dark-matter:171" "Memory Scanner" [Interface] Set.StrangeMoons

realitySimulator :: CardDef
realitySimulator =
  location_ ":dark-matter:172" "Reality Simulator" [] Set.StrangeMoons

aHidingPlace :: CardDef
aHidingPlace =
  victory 1 $ location_ ":dark-matter:173" "A Hiding Place" [Simulation] Set.StrangeMoons

aMutiny :: CardDef
aMutiny =
  location_ ":dark-matter:174" "A Mutiny" [Memory, Simulation] Set.StrangeMoons

adriftInSpace :: CardDef
adriftInSpace =
  location_ ":dark-matter:175" "Adrift in Space" [Simulation, Nightmare] Set.StrangeMoons

anAccident :: CardDef
anAccident =
  victory 1 $ location_ ":dark-matter:176" "An Accident" [Memory, Simulation] Set.StrangeMoons

cityOfCats :: CardDef
cityOfCats =
  victory 1
    $ location_ ":dark-matter:177" "City of Cats" [Simulation, Dreamlands] Set.StrangeMoons

feverDream :: CardDef
feverDream =
  victory 1
    $ location_ ":dark-matter:178" "Fever Dream" [Simulation, Nightmare] Set.StrangeMoons

-- fragment_of_carcosa
abandonedLander :: CardDef
abandonedLander =
  location_ ":dark-matter:219" "Abandoned Lander" [Surface] Set.FragmentOfCarcosa

surfaceOfFragment :: CardDef
surfaceOfFragment =
  location_ ":dark-matter:220" "Surface of Fragment" [Surface] Set.FragmentOfCarcosa

bottomlessPit :: CardDef
bottomlessPit =
  location_ ":dark-matter:221" "Bottomless Pit" [Cave] Set.FragmentOfCarcosa

cyclopeanCaverns :: CardDef
cyclopeanCaverns =
  location_ ":dark-matter:222" "Cyclopean Caverns" [Cave] Set.FragmentOfCarcosa

hiddenPassage :: CardDef
hiddenPassage =
  location_ ":dark-matter:223" "Hidden Passage" [Surface, Cave] Set.FragmentOfCarcosa

iceCavity :: CardDef
iceCavity = location_ ":dark-matter:224" "Ice Cavity" [Cave] Set.FragmentOfCarcosa

impassableRavine :: CardDef
impassableRavine =
  victory 1
    $ location_
      ":dark-matter:225"
      ("Impassable Ravine" <:> "Where the Cultists Disappeared")
      [Cave]
      Set.FragmentOfCarcosa

stalagmiteForest :: CardDef
stalagmiteForest =
  location_ ":dark-matter:226" "Stalagmite Forest" [Cave] Set.FragmentOfCarcosa

-- starfall
theTatterdemalion :: CardDef
theTatterdemalion =
  location_ ":dark-matter:253" "The Tatterdemalion" [Starship] Set.Starfall

newBrooklyn :: CardDef
newBrooklyn =
  location_
    ":dark-matter:254"
    ("New Brooklyn" <:> "Population: 4034 Humans")
    [Colony, AsteroidBelt]
    Set.Starfall

hope :: CardDef
hope =
  location_
    ":dark-matter:255"
    ("Hope" <:> "Population: 138021 Humans")
    [Colony, Mars]
    Set.Starfall

yuggoth :: CardDef
yuggoth =
  victory 1
    $ location_
      ":dark-matter:256"
      ("Yuggoth" <:> "Population: 23615 Mi-Go")
      [Colony, Pluto]
      Set.Starfall

theCassilda :: CardDef
theCassilda = location_ ":dark-matter:258" "The Cassilda" [Starship] Set.Starfall

earth :: CardDef
earth =
  victory 2
    $ location_ ":dark-matter:262" ("Earth" <:> "The Stars Were Right") [Earth] Set.Starfall

mountSinai :: CardDef
mountSinai =
  victory 1 $ location_ ":dark-matter:263" "Mount Sinai" [AsteroidBelt] Set.Starfall

derelictShip :: CardDef
derelictShip = location_ ":dark-matter:264" "Derelict Ship" [Starship] Set.Starfall

martianRuins :: CardDef
martianRuins =
  victory 1 $ location_ ":dark-matter:265" "Martian Ruins" [Mars] Set.Starfall

olympusTelescope :: CardDef
olympusTelescope =
  location_ ":dark-matter:266" "Olympus Telescope" [Mars] Set.Starfall

moonbaseLaboratory :: CardDef
moonbaseLaboratory =
  victory 1
    $ location_ ":dark-matter:267" "Moonbase Laboratory" [Colony, Trait.Moon] Set.Starfall

thresholdOfYuggoth :: CardDef
thresholdOfYuggoth =
  victory 1 $ location_ ":dark-matter:268" "Threshold of Yuggoth" [Pluto] Set.Starfall
