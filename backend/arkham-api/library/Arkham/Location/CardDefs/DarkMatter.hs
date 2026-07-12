module Arkham.Location.CardDefs.DarkMatter where

import Arkham.Location.CardDefs.Import
import Arkham.Trait qualified as Trait

-- the_tatterdemalion
cargoHoldDarkMatter :: CardDef
cargoHoldDarkMatter =
  location_ "z-dark-matter-021" "Cargo Hold" [Tatterdemalion, Access] DarkMatterTheTatterdemalion

cryosleepQuartersDarkMatter :: CardDef
cryosleepQuartersDarkMatter =
  location_ "z-dark-matter-022" "Cryosleep Quarters" [Tatterdemalion] DarkMatterTheTatterdemalion

engineRoomDarkMatter_023 :: CardDef
engineRoomDarkMatter_023 =
  location_ "z-dark-matter-023" "Engine Room" [Tatterdemalion] DarkMatterTheTatterdemalion

escapePodBayDarkMatter :: CardDef
escapePodBayDarkMatter =
  location_ "z-dark-matter-024" "Escape Pod Bay" [Tatterdemalion, Access] DarkMatterTheTatterdemalion

infirmaryDarkMatter_025 :: CardDef
infirmaryDarkMatter_025 =
  location_ "z-dark-matter-025" "Infirmary" [Tatterdemalion] DarkMatterTheTatterdemalion

messHallDarkMatter :: CardDef
messHallDarkMatter =
  location_ "z-dark-matter-026" "Mess Hall" [Tatterdemalion] DarkMatterTheTatterdemalion

shipsBridgeDarkMatter :: CardDef
shipsBridgeDarkMatter =
  location_ "z-dark-matter-027" "Ship's Bridge" [Tatterdemalion, Access] DarkMatterTheTatterdemalion

ventilationShaftDarkMatter :: CardDef
ventilationShaftDarkMatter =
  location_ "z-dark-matter-040" "Ventilation Shaft" [Tatterdemalion] DarkMatterTheTatterdemalion

-- electric_nightmare
cafeteriaDarkMatter :: CardDef
cafeteriaDarkMatter = location_ "z-dark-matter-071" "Cafeteria" [School] DarkMatterElectricNightmare

classroomK2DarkMatter :: CardDef
classroomK2DarkMatter =
  victory 1 $ location_ "z-dark-matter-072" "Classroom K2" [School] DarkMatterElectricNightmare

entranceHallDarkMatter :: CardDef
entranceHallDarkMatter =
  location_ "z-dark-matter-073" "Entrance Hall" [School] DarkMatterElectricNightmare

gymnasiumDarkMatter :: CardDef
gymnasiumDarkMatter = location_ "z-dark-matter-074" "Gymnasium" [School] DarkMatterElectricNightmare

biologyLabDarkMatter :: CardDef
biologyLabDarkMatter =
  location_ "z-dark-matter-075" "Biology Lab" [School] DarkMatterElectricNightmare

libraryDarkMatter :: CardDef
libraryDarkMatter =
  victory 1 $ location_ "z-dark-matter-076" "Library" [School] DarkMatterElectricNightmare

schoolGroundsDarkMatter :: CardDef
schoolGroundsDarkMatter =
  victory 1 $ location_ "z-dark-matter-077" "School Grounds" [] DarkMatterElectricNightmare

-- lost_quantum
coldWastesDarkMatter :: CardDef
coldWastesDarkMatter = location_ "z-dark-matter-099" "Cold Wastes" [Elbrus] DarkMatterLostQuantum

crystalPeakDarkMatter :: CardDef
crystalPeakDarkMatter =
  victory 2 $ location_ "z-dark-matter-100" "Crystal Peak" [Elbrus, Extradimensional] DarkMatterLostQuantum

iceSpiresDarkMatter :: CardDef
iceSpiresDarkMatter =
  victory 1 $ location_ "z-dark-matter-101" "Ice Spires" [Elbrus] DarkMatterLostQuantum

landingCraftDarkMatter :: CardDef
landingCraftDarkMatter =
  location_ "z-dark-matter-102" "Landing Craft" [Elbrus] DarkMatterLostQuantum

mainFacilityDarkMatter :: CardDef
mainFacilityDarkMatter =
  victory 1 $ location_ "z-dark-matter-103" "Main Facility" [Elbrus] DarkMatterLostQuantum

omniTransmittersDarkMatter :: CardDef
omniTransmittersDarkMatter =
  location_ "z-dark-matter-104" "Omni-Transmitters" [Elbrus] DarkMatterLostQuantum

qCrystalMinesDarkMatter :: CardDef
qCrystalMinesDarkMatter =
  victory 1 $ location_ "z-dark-matter-105" "Q-Crystal Mines" [Elbrus] DarkMatterLostQuantum

schrodGeneratorsDarkMatter :: CardDef
schrodGeneratorsDarkMatter =
  location_ "z-dark-matter-106" "Schröd Generators" [Elbrus] DarkMatterLostQuantum

-- in_the_shadow_of_earth
airlocksDarkMatter :: CardDef
airlocksDarkMatter =
  location_ "z-dark-matter-125" "Airlocks" [NostalgiaII] DarkMatterInTheShadowOfEarth

crewQuartersDarkMatter :: CardDef
crewQuartersDarkMatter =
  location_ "z-dark-matter-126" "Crew Quarters" [NostalgiaII] DarkMatterInTheShadowOfEarth

engineRoomDarkMatter_127 :: CardDef
engineRoomDarkMatter_127 =
  victory 1 $ location_ "z-dark-matter-127" "Engine Room" [NostalgiaII] DarkMatterInTheShadowOfEarth

flightDeckDarkMatter :: CardDef
flightDeckDarkMatter =
  victory 1 $ location_ "z-dark-matter-128" "Flight Deck" [NostalgiaII] DarkMatterInTheShadowOfEarth

hydroponicsDarkMatter :: CardDef
hydroponicsDarkMatter =
  location_ "z-dark-matter-129" "Hydroponics" [NostalgiaII] DarkMatterInTheShadowOfEarth

infirmaryDarkMatter_130 :: CardDef
infirmaryDarkMatter_130 =
  location_ "z-dark-matter-130" "Infirmary" [NostalgiaII] DarkMatterInTheShadowOfEarth

shipMainframeDarkMatter :: CardDef
shipMainframeDarkMatter =
  location_ "z-dark-matter-131" "Ship Mainframe" [NostalgiaII] DarkMatterInTheShadowOfEarth

telecomsDarkMatter :: CardDef
telecomsDarkMatter =
  location_ "z-dark-matter-132" "Telecoms" [NostalgiaII] DarkMatterInTheShadowOfEarth

-- strange_moons
brainStorageDarkMatter :: CardDef
brainStorageDarkMatter =
  location_ "z-dark-matter-167" "Brain Storage" [Interface] DarkMatterStrangeMoons

communicatorDarkMatter :: CardDef
communicatorDarkMatter =
  location_ "z-dark-matter-168" "Communicator" [Interface] DarkMatterStrangeMoons

dreamDiagnosticsDarkMatter :: CardDef
dreamDiagnosticsDarkMatter =
  location_ "z-dark-matter-169" "Dream Diagnostics" [Interface] DarkMatterStrangeMoons

entranceTunnelDarkMatter :: CardDef
entranceTunnelDarkMatter = location_ "z-dark-matter-170" "Entrance Tunnel" [] DarkMatterStrangeMoons

memoryScannerDarkMatter :: CardDef
memoryScannerDarkMatter =
  location_ "z-dark-matter-171" "Memory Scanner" [Interface] DarkMatterStrangeMoons

realitySimulatorDarkMatter :: CardDef
realitySimulatorDarkMatter =
  location_ "z-dark-matter-172" "Reality Simulator" [] DarkMatterStrangeMoons

aHidingPlaceDarkMatter :: CardDef
aHidingPlaceDarkMatter =
  victory 1 $ location_ "z-dark-matter-173" "A Hiding Place" [Simulation] DarkMatterStrangeMoons

aMutinyDarkMatter :: CardDef
aMutinyDarkMatter =
  location_ "z-dark-matter-174" "A Mutiny" [Memory, Simulation] DarkMatterStrangeMoons

adriftInSpaceDarkMatter :: CardDef
adriftInSpaceDarkMatter =
  location_ "z-dark-matter-175" "Adrift in Space" [Simulation, Nightmare] DarkMatterStrangeMoons

anAccidentDarkMatter :: CardDef
anAccidentDarkMatter =
  victory 1 $ location_ "z-dark-matter-176" "An Accident" [Memory, Simulation] DarkMatterStrangeMoons

cityOfCatsDarkMatter :: CardDef
cityOfCatsDarkMatter =
  victory 1 $ location_ "z-dark-matter-177" "City of Cats" [Simulation, Dreamlands] DarkMatterStrangeMoons

feverDreamDarkMatter :: CardDef
feverDreamDarkMatter =
  victory 1 $ location_ "z-dark-matter-178" "Fever Dream" [Simulation, Nightmare] DarkMatterStrangeMoons

-- fragment_of_carcosa
abandonedLanderDarkMatter :: CardDef
abandonedLanderDarkMatter =
  location_ "z-dark-matter-219" "Abandoned Lander" [Surface] DarkMatterFragmentOfCarcosa

surfaceOfFragmentDarkMatter :: CardDef
surfaceOfFragmentDarkMatter =
  location_ "z-dark-matter-220" "Surface of Fragment" [Surface] DarkMatterFragmentOfCarcosa

bottomlessPitDarkMatter :: CardDef
bottomlessPitDarkMatter =
  location_ "z-dark-matter-221" "Bottomless Pit" [Cave] DarkMatterFragmentOfCarcosa

cyclopeanCavernsDarkMatter :: CardDef
cyclopeanCavernsDarkMatter =
  location_ "z-dark-matter-222" "Cyclopean Caverns" [Cave] DarkMatterFragmentOfCarcosa

hiddenPassageDarkMatter :: CardDef
hiddenPassageDarkMatter =
  location_ "z-dark-matter-223" "Hidden Passage" [Surface, Cave] DarkMatterFragmentOfCarcosa

iceCavityDarkMatter :: CardDef
iceCavityDarkMatter = location_ "z-dark-matter-224" "Ice Cavity" [Cave] DarkMatterFragmentOfCarcosa

impassableRavineDarkMatter :: CardDef
impassableRavineDarkMatter =
  victory 1 $ location_ "z-dark-matter-225" ("Impassable Ravine" <:> "Where the Cultists Disappeared") [Cave] DarkMatterFragmentOfCarcosa

stalagmiteForestDarkMatter :: CardDef
stalagmiteForestDarkMatter =
  location_ "z-dark-matter-226" "Stalagmite Forest" [Cave] DarkMatterFragmentOfCarcosa

-- starfall
theTatterdemalionDarkMatter :: CardDef
theTatterdemalionDarkMatter =
  location_ "z-dark-matter-253" "The Tatterdemalion" [Starship] DarkMatterStarfall

newBrooklynDarkMatter :: CardDef
newBrooklynDarkMatter =
  location_ "z-dark-matter-254" ("New Brooklyn" <:> "Population: 4034 Humans") [Colony, AsteroidBelt] DarkMatterStarfall

hopeDarkMatter :: CardDef
hopeDarkMatter =
  location_ "z-dark-matter-255" ("Hope" <:> "Population: 138021 Humans") [Colony, Mars] DarkMatterStarfall

yuggothDarkMatter :: CardDef
yuggothDarkMatter =
  victory 1 $ location_ "z-dark-matter-256" ("Yuggoth" <:> "Population: 23615 Mi-Go") [Colony, Pluto] DarkMatterStarfall

theCassildaDarkMatter :: CardDef
theCassildaDarkMatter = location_ "z-dark-matter-258" "The Cassilda" [Starship] DarkMatterStarfall

earthDarkMatter :: CardDef
earthDarkMatter =
  victory 2 $ location_ "z-dark-matter-262" ("Earth" <:> "The Stars Were Right") [Earth] DarkMatterStarfall

mountSinaiDarkMatter :: CardDef
mountSinaiDarkMatter =
  victory 1 $ location_ "z-dark-matter-263" "Mount Sinai" [AsteroidBelt] DarkMatterStarfall

derelictShipDarkMatter :: CardDef
derelictShipDarkMatter = location_ "z-dark-matter-264" "Derelict Ship" [Starship] DarkMatterStarfall

martianRuinsDarkMatter :: CardDef
martianRuinsDarkMatter =
  victory 1 $ location_ "z-dark-matter-265" "Martian Ruins" [Mars] DarkMatterStarfall

olympusTelescopeDarkMatter :: CardDef
olympusTelescopeDarkMatter =
  location_ "z-dark-matter-266" "Olympus Telescope" [Mars] DarkMatterStarfall

moonbaseLaboratoryDarkMatter :: CardDef
moonbaseLaboratoryDarkMatter =
  victory 1 $ location_ "z-dark-matter-267" "Moonbase Laboratory" [Colony, Trait.Moon] DarkMatterStarfall

thresholdOfYuggothDarkMatter :: CardDef
thresholdOfYuggothDarkMatter =
  victory 1 $ location_ "z-dark-matter-268" "Threshold of Yuggoth" [Pluto] DarkMatterStarfall
