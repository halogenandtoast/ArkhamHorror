module Arkham.Location.CardDefs.Standalone where

import Arkham.Location.CardDefs.Import

lobbyTheMidwinterGala :: CardDef
lobbyTheMidwinterGala =
  location
    "71007"
    "Lobby"
    [Manor, GroundFloor]
    Diamond
    [Moon, Spade, Triangle, Square, Star]
    TheMidwinterGala

lanternChamber :: CardDef
lanternChamber =
  victory 1
    $ location "71008" "Lantern Chamber" [Manor, Private, Basement] Star [Diamond] TheMidwinterGala

artGalleryTheMidwinterGala :: CardDef
artGalleryTheMidwinterGala =
  locationWithUnrevealed
    "71009"
    "Ground-Floor Room"
    [Manor, GroundFloor]
    Moon
    []
    "Art Gallery"
    [Manor, GroundFloor]
    Triangle
    [Diamond, Spade, Moon, Square]
    TheMidwinterGala

ballroomTheMidwinterGala :: CardDef
ballroomTheMidwinterGala =
  locationWithUnrevealed
    "71010"
    "Ground-Floor Room"
    [Manor, GroundFloor]
    Moon
    []
    "Ballroom"
    [Manor, GroundFloor]
    Square
    [Diamond, Spade, Triangle, Moon]
    TheMidwinterGala

barroom :: CardDef
barroom =
  locationWithUnrevealed
    "71011"
    "Ground-Floor Room"
    [Manor, GroundFloor]
    Moon
    []
    "Barroom"
    [Manor, GroundFloor]
    Spade
    [Diamond, Moon, Triangle, Square]
    TheMidwinterGala

bedroomTheMidwinterGala :: CardDef
bedroomTheMidwinterGala =
  locationWithUnrevealed
    "71012"
    "Second-Floor Room"
    [Manor, Private, SecondFloor]
    Heart
    []
    "Bedroom"
    [Manor, Private, SecondFloor]
    T
    [Circle, Hourglass, Heart]
    TheMidwinterGala

libraryTheMidwinterGala :: CardDef
libraryTheMidwinterGala =
  victory 1
    $ locationWithUnrevealed
      "71013"
      "Second-Floor Room"
      [Manor, Private, SecondFloor]
      Heart
      []
      "Library"
      [Manor, Private, SecondFloor]
      Hourglass
      [Circle, T, Heart]
      TheMidwinterGala

parlorTheMidwinterGala :: CardDef
parlorTheMidwinterGala =
  victory 1
    $ locationWithUnrevealed
      "71014"
      "Second-Floor Room"
      [Manor, Private, SecondFloor]
      Heart
      []
      "Parlor"
      [Manor, Private, SecondFloor]
      Circle
      [T, Hourglass, Heart]
      TheMidwinterGala

centralLotQuietOnSet :: CardDef
centralLotQuietOnSet =
  otherSideIs "72008b"
    $ location
      "72008"
      ("Central Lot" <:> "Quiet on Set")
      [Set, Central]
      Circle
      [Moon, Diamond, Triangle]
      FilmFatale

centralLotBlurred :: CardDef
centralLotBlurred =
  otherSideIs "72008"
    $ location
      "72008b"
      ("Central Lot" <:> "Blurred")
      [Set, Central, Extradimensional]
      Circle
      [Moon, Diamond, Triangle]
      FilmFatale

spaceSet :: CardDef
spaceSet = location "72009" "Space Set" [Set] Moon [Circle] FilmFatale

jungleSet :: CardDef
jungleSet = location "72010" "Jungle Set" [Set] Diamond [Circle, Droplet] FilmFatale

gothicSet :: CardDef
gothicSet =
  location
    "72011"
    "Gothic Set"
    [Set]
    Triangle
    [Circle, Heart, Hourglass, Trefoil, Square, Equals]
    FilmFatale

highRulersBastion :: CardDef
highRulersBastion = location "72027" "High Ruler's Bastion" [Cosmos] NoSymbol [] CosmicJourney

teetawnPassage :: CardDef
teetawnPassage = victory 1 $ location "72028" "Teetawn Passage" [Cosmos] Droplet [Squiggle] CosmicJourney

ritualSiteTeetawn :: CardDef
ritualSiteTeetawn = location "72029" "Ritual Site Teetawn" [RitualSite] Squiggle [Droplet] CosmicJourney

tothisBarrens :: CardDef
tothisBarrens = victory 1 $ location "72030" "Tothis Barrens" [Cosmos] T [Star] CosmicJourney

ritualSiteTothis :: CardDef
ritualSiteTothis = location "72031" "Ritual Site Tothis" [RitualSite] Star [T] CosmicJourney

lostAsteroid :: CardDef
lostAsteroid =
  quantity 2
    $ singleSided
    $ revelation
    $ location "72034" "Lost Asteroid" [Cosmos] NoSymbol [] CosmicJourney

westernRidge :: CardDef
westernRidge = location "72039" "Western Ridge" [Jungle] Droplet [Diamond, T, Plus] ForgottenIsland

tarPit :: CardDef
tarPit = victory 1 $ location "72040" "Tar Pit" [Jungle] T [Droplet, Hourglass] ForgottenIsland

easternRidge :: CardDef
easternRidge = location "72041" "Eastern Ridge" [Jungle] Hourglass [T, Plus] ForgottenIsland

jungleRiver :: CardDef
jungleRiver = location "72042" "Jungle River" [Jungle] Plus [Droplet, Hourglass, Squiggle] ForgottenIsland

ruinsOfTheSerpentKing :: CardDef
ruinsOfTheSerpentKing =
  victory 1
    $ location "72043" "Ruins of the Serpent King" [Jungle, Ruins] Squiggle [Plus] ForgottenIsland

castleHallwaysSeeminglyEndless :: CardDef
castleHallwaysSeeminglyEndless =
  location
    "72050"
    ("Castle Hallways" <:> "Seemingly Endless")
    [Castle]
    Heart
    [Triangle, Square]
    AbominableContessa

catacombsStinksOfDeath :: CardDef
catacombsStinksOfDeath =
  location
    "72051"
    ("Catacombs" <:> "Stinks of Death")
    [Castle]
    Hourglass
    [Triangle, Trefoil]
    AbominableContessa

clockTowerIncessantlyTicking :: CardDef
clockTowerIncessantlyTicking =
  victory 1
    $ location
      "72052"
      ("Clock Tower" <:> "Incessantly Ticking")
      [Castle]
      Trefoil
      [Triangle, Hourglass]
      AbominableContessa

moonlitGardenPoisonedBeauty :: CardDef
moonlitGardenPoisonedBeauty =
  location
    "72053"
    ("Moonlit Garden" <:> "Poisoned Beauty")
    [Castle]
    Square
    [Triangle, Heart]
    AbominableContessa

throneOfBloodRedAsBloodBlackAsNight :: CardDef
throneOfBloodRedAsBloodBlackAsNight =
  victory 1
    $ location
      "72054"
      ("Throne of Blood" <:> "Red as Blood, Black as Night")
      [Castle, Sanctum]
      Equals
      [Triangle]
      AbominableContessa

cursedShores :: CardDef
cursedShores =
  location
    "81007"
    "Cursed Shores"
    [NewOrleans, Bayou]
    Square
    [Plus, Triangle, Diamond, Hourglass]
    TheBayou

gardenDistrict :: CardDef
gardenDistrict =
  locationWithUnrevealed
    "81008"
    "New Orleans"
    [NewOrleans]
    Plus
    [Square, Plus]
    "Garden District"
    [NewOrleans]
    Plus
    [Square, Plus]
    TheBayou

broadmoor :: CardDef
broadmoor =
  victory 1
    $ locationWithUnrevealed
      "81009"
      "New Orleans"
      [NewOrleans]
      Plus
      [Square, Plus]
      "Broadmoor"
      [NewOrleans]
      Plus
      [Square, Plus]
      TheBayou

brackishWaters :: CardDef
brackishWaters =
  location
    "81010"
    "Brackish Waters"
    [Riverside, Bayou]
    Triangle
    [Squiggle, Square, Diamond, Hourglass]
    TheBayou

audubonPark :: CardDef
audubonPark =
  victory 1
    $ locationWithUnrevealed
      "81011"
      "Riverside"
      [Riverside]
      Squiggle
      [Triangle, Squiggle]
      "Audubon Park"
      [Riverside]
      Squiggle
      [Triangle, Squiggle]
      TheBayou

faubourgMarigny :: CardDef
faubourgMarigny =
  locationWithUnrevealed
    "81012"
    "Riverside"
    [Riverside]
    Squiggle
    [Triangle, Squiggle]
    "Faubourg Marigny"
    [Riverside]
    Squiggle
    [Triangle, Squiggle]
    TheBayou

forgottenMarsh :: CardDef
forgottenMarsh =
  location
    "81013"
    "Forgotten Marsh"
    [Wilderness, Bayou]
    Diamond
    [Moon, Square, Triangle, Hourglass]
    TheBayou

trappersCabin :: CardDef
trappersCabin =
  locationWithUnrevealed
    "81014"
    "Wilderness"
    [Wilderness]
    Moon
    [Diamond, Moon]
    "Trapper's Cabin"
    [Wilderness]
    Moon
    [Diamond, Moon]
    TheBayou

twistedUnderbrush :: CardDef
twistedUnderbrush =
  victory 1
    $ locationWithUnrevealed
      "81015"
      "Wilderness"
      [Wilderness]
      Moon
      [Diamond, Moon]
      "Twisted Underbrush"
      [Wilderness]
      Moon
      [Diamond, Moon]
      TheBayou

foulSwamp :: CardDef
foulSwamp =
  location
    "81016"
    "Foul Swamp"
    [Unhallowed, Bayou]
    Hourglass
    [Equals, Square, Triangle, Diamond]
    TheBayou

ritualGrounds :: CardDef
ritualGrounds =
  victory 1
    $ locationWithUnrevealed
      "81017"
      "Unhallowed Land"
      [Unhallowed]
      Equals
      [Hourglass, Equals]
      "Ritual Grounds"
      [Unhallowed]
      Equals
      [Hourglass, Equals]
      TheBayou

overgrownCairns :: CardDef
overgrownCairns =
  locationWithUnrevealed
    "81018"
    "Unhallowed Land"
    [Unhallowed]
    Equals
    [Hourglass, Equals]
    "Overgrown Cairns"
    [Unhallowed]
    Equals
    [Hourglass, Equals]
    TheBayou

gondola :: CardDef
gondola =
  location "82006b" "Gondola" [Venice, Boat] NoSymbol [] CarnevaleOfHorrors
    & otherSideIs "82006"

sanMarcoBasilica :: CardDef
sanMarcoBasilica =
  location "82008" "San Marco Basilica" [Venice] NoSymbol [] CarnevaleOfHorrors

canalSide :: CardDef
canalSide =
  location "82009" "Canal-side" [Venice] NoSymbol [] CarnevaleOfHorrors

streetsOfVenice :: CardDef
streetsOfVenice =
  location "82010" "Streets of Venice" [Venice] NoSymbol [] CarnevaleOfHorrors

rialtoBridge :: CardDef
rialtoBridge =
  location
    "82011"
    "Rialto Bridge"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

venetianGarden :: CardDef
venetianGarden =
  location "82012" "Venetian Garden" [Venice] NoSymbol [] CarnevaleOfHorrors

bridgeOfSighs :: CardDef
bridgeOfSighs =
  location
    "82013"
    "Bridge of Sighs"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

floodedSquare :: CardDef
floodedSquare =
  location "82014" "Flooded Square" [Venice] NoSymbol [] CarnevaleOfHorrors

accademiaBridge :: CardDef
accademiaBridge =
  location
    "82015"
    "Accademia Bridge"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

theGuardian :: CardDef
theGuardian =
  location "82016" "The Guardian" [Venice] NoSymbol [] CarnevaleOfHorrors

room225 :: CardDef
room225 =
  location
    "84010"
    ("Room 225" <:> "Scene of the Crime")
    [CrimeScene]
    Circle
    [Square, Triangle]
    MurderAtTheExcelsiorHotel

suiteBalcony :: CardDef
suiteBalcony =
  location
    "84011"
    "Suite Balcony"
    [CrimeScene]
    Triangle
    [Circle]
    MurderAtTheExcelsiorHotel

secondFloorHall :: CardDef
secondFloorHall =
  location
    "84012"
    "Second Floor Hall"
    [Hall]
    Square
    [Circle, T, Squiggle, Plus, Diamond, Equals]
    MurderAtTheExcelsiorHotel

foyerMurderAtTheExcelsiorHotel :: CardDef
foyerMurderAtTheExcelsiorHotel =
  location
    "84013"
    "Foyer"
    [Hall]
    T
    [Square, Squiggle, Hourglass, Moon]
    MurderAtTheExcelsiorHotel

restaurant :: CardDef
restaurant =
  location
    "84014"
    "Restaurant"
    [Hall]
    Squiggle
    [Square, T]
    MurderAtTheExcelsiorHotel

hotelRoof :: CardDef
hotelRoof =
  victory 1
    $ location
      "84015"
      "Hotel Roof"
      []
      Plus
      [Square]
      MurderAtTheExcelsiorHotel

room212 :: CardDef
room212 =
  victory 1
    $ location
      "84016"
      "Room 212"
      [CrimeScene]
      Diamond
      [Square]
      MurderAtTheExcelsiorHotel

room245 :: CardDef
room245 =
  victory 1
    $ location
      "84017"
      "Room 245"
      [CrimeScene]
      Equals
      [Square]
      MurderAtTheExcelsiorHotel

officeMurderAtTheExcelsiorHotel :: CardDef
officeMurderAtTheExcelsiorHotel =
  victory 1
    $ location
      "84018"
      "Office"
      []
      Moon
      [T, Hourglass]
      MurderAtTheExcelsiorHotel

basement :: CardDef
basement =
  victory 1
    $ location
      "84019"
      "Basement"
      [CrimeScene]
      Hourglass
      [T, Moon]
      MurderAtTheExcelsiorHotel

casinoFloorCalmNight :: CardDef
casinoFloorCalmNight =
  otherSideIs "88009b"
    $ location
      "88009"
      ("Casino Floor" <:> "Calm Night")
      [Public, Casino]
      Circle
      [Square, Diamond]
      FortuneAndFolly

casinoFloorBusyNight :: CardDef
casinoFloorBusyNight =
  otherSideIs "88009"
    $ location
      "88009b"
      ("Casino Floor" <:> "Busy Night")
      [Public, Casino]
      Circle
      [Square, Diamond]
      FortuneAndFolly

pokerTable :: CardDef
pokerTable =
  location
    "88010"
    "Poker Table"
    [Public, Casino, Game]
    Square
    [Plus, Circle]
    FortuneAndFolly

rouletteWheel :: CardDef
rouletteWheel =
  location
    "88011"
    "Roulette Wheel"
    [Public, Casino, Game]
    Triangle
    [Diamond, T, Plus]
    FortuneAndFolly

baccaratTable :: CardDef
baccaratTable =
  location
    "88012"
    "Baccarat Table"
    [Public, Casino, Game]
    Plus
    [Squiggle, Square, Triangle]
    FortuneAndFolly

slotMachines :: CardDef
slotMachines =
  location
    "88013"
    "Slot Machines"
    [Public, Casino, Game]
    Diamond
    [Circle, Triangle]
    FortuneAndFolly

highRollersTableCalmNight :: CardDef
highRollersTableCalmNight =
  otherSideIs "88014b"
    $ location
      "88014"
      ("High Roller's Table" <:> "Calm Night")
      [Public, Casino, Game]
      Squiggle
      [T, Plus, Hourglass]
      FortuneAndFolly

highRollersTableBusyNight :: CardDef
highRollersTableBusyNight =
  otherSideIs "88014"
    $ location
      "88014b"
      ("High Roller's Table" <:> "Busy Night")
      [Public, Casino, Game]
      Squiggle
      [T, Plus, Hourglass]
      FortuneAndFolly

casinoLoungeCalmNight :: CardDef
casinoLoungeCalmNight =
  otherSideIs "88015b"
    $ location
      "88015"
      ("Casino Lounge" <:> "Calm Night")
      [Public, Casino]
      T
      [Triangle, Squiggle, Hourglass]
      FortuneAndFolly

casinoLoungeBusyNight :: CardDef
casinoLoungeBusyNight =
  victory 1
    $ otherSideIs "88015"
    $ location
      "88015b"
      ("Casino Lounge" <:> "Busy Night")
      [Public, Casino]
      T
      [Triangle, Squiggle, Hourglass]
      FortuneAndFolly

staffAccessHallway :: CardDef
staffAccessHallway =
  location
    "88016"
    "Staff Access Hallway"
    [Restricted, Casino]
    Hourglass
    [Moon, Equals, Squiggle, T]
    FortuneAndFolly

securityOffice :: CardDef
securityOffice =
  victory 1
    $ location
      "88017"
      "Security Office"
      [Restricted, Casino]
      Moon
      [Heart, Hourglass, Equals]
      FortuneAndFolly

guardRoom :: CardDef
guardRoom =
  location
    "88018"
    "Guard Room"
    [Restricted, Casino]
    Equals
    [Hourglass, Star, Moon]
    FortuneAndFolly

ownersOffice :: CardDef
ownersOffice =
  victory 1
    $ location
      "88019"
      "Owner's Office"
      [Restricted, Casino]
      Heart
      [Star, Moon, Droplet]
      FortuneAndFolly

countingRoom :: CardDef
countingRoom =
  location
    "88020"
    "Counting Room"
    [Restricted, Casino]
    Star
    [Equals, Heart, Droplet]
    FortuneAndFolly

vaultDoor :: CardDef
vaultDoor =
  location
    "88021"
    "Vault Door"
    [Restricted, Casino]
    Droplet
    [Star, Heart, Trefoil]
    FortuneAndFolly

relicRoomSanctumOfFortune :: CardDef
relicRoomSanctumOfFortune =
  location
    "88022"
    ("Relic Room" <:> "Sanctum of Fortune")
    [Restricted, Casino]
    Trefoil
    [Droplet]
    FortuneAndFolly

-- The Blob That Ate Everything

theCrater :: CardDef
theCrater = location_ "85010" "The Crater" [Oozified] TheBlobThatAteEverything

researchSiteTheBlobThatAteEverything :: CardDef
researchSiteTheBlobThatAteEverything =
  location_ "85011" "Research Site" [] TheBlobThatAteEverything

temporaryHQ :: CardDef
temporaryHQ = location_ "85012" "Temporary HQ" [] TheBlobThatAteEverything

fungusMound :: CardDef
fungusMound = location_ "85013" "Fungus Mound" [] TheBlobThatAteEverything

sewer :: CardDef
sewer = location_ "85014" "Sewer" [Oozified] TheBlobThatAteEverything

bridge :: CardDef
bridge = location_ "85015" "Bridge" [Oozified] TheBlobThatAteEverything

waterTower :: CardDef
waterTower = location_ "85016" "Water Tower" [Oozified] TheBlobThatAteEverything

church :: CardDef
church = location_ "85017" "Church" [Oozified] TheBlobThatAteEverything

oozyLakebed :: CardDef
oozyLakebed =
  (location_ "85018" "Oozy Lakebed" [Oozified] TheBlobThatAteEverything)
    { cdEncounterSetQuantity = Just 2
    }

slimyStreets :: CardDef
slimyStreets =
  (location_ "85019" "Slimy Streets" [Oozified] TheBlobThatAteEverything)
    { cdEncounterSetQuantity = Just 2
    }

desiccatedFarmland :: CardDef
desiccatedFarmland =
  (location_ "85020" "Desiccated Farmland" [Oozified] TheBlobThatAteEverything)
    { cdEncounterSetQuantity = Just 2
    }

streetsOfCairo :: CardDef
streetsOfCairo =
  singleSided
    $ location "83008" "Streets of Cairo" [Cairo] Equals [Diamond, Triangle, Square, Trefoil] TheEternalSlumber

cairoBazaar :: CardDef
cairoBazaar =
  singleSided
    $ location "83009" "Cairo Bazaar" [Cairo] Square [Triangle, Equals, Trefoil] TheEternalSlumber

museumOfEgyptianAntiquities :: CardDef
museumOfEgyptianAntiquities =
  singleSided
    $ location
      "83010"
      "Museum of Egyptian Antiquities"
      [Cairo]
      Triangle
      [Diamond, Equals, Square]
      TheEternalSlumber

outskirtsOfCairo :: CardDef
outskirtsOfCairo =
  singleSided
    $ location
      "83011"
      "Outskirts of Cairo"
      [Cairo]
      Diamond
      [Circle, Triangle, Equals, Trefoil]
      TheEternalSlumber

templeCourtyard :: CardDef
templeCourtyard =
  singleSided
    $ location "83012" "Temple Courtyard" [Cairo] Trefoil [Diamond, Equals, Square] TheEternalSlumber

aDreamBetwixt :: CardDef
aDreamBetwixt =
  otherSideIs "83022b"
    $ location "83022a" "A Dream Betwixt" [Otherworld, Extradimensional] Equals [Square] TheNightsUsurper

theGreatAbyss :: CardDef
theGreatAbyss =
  victory 1
    $ otherSideIs "83023b"
    $ location
      "83023a"
      "The Great Abyss"
      [Otherworld, Dreamlands]
      Square
      [Equals, Triangle, Diamond, Circle]
      TheNightsUsurper

tunnelsUnderNgranek :: CardDef
tunnelsUnderNgranek =
  otherSideIs "83024b"
    $ location "83024a" "Tunnels under Ngranek" [Otherworld, Dreamlands] Triangle [Square] TheNightsUsurper

stairwayToSarkomand :: CardDef
stairwayToSarkomand =
  otherSideIs "83025b"
    $ location "83025a" "Stairway to Sarkomand" [Otherworld, Dreamlands] Diamond [Square] TheNightsUsurper

mistFilledCaverns :: CardDef
mistFilledCaverns =
  otherSideIs "83026b"
    $ location "83026a" "Mist-Filled Caverns" [Otherworld, Dreamlands] Circle [Square] TheNightsUsurper

eldritchGate :: CardDef
eldritchGate =
  singleSided
    $ location "83028" "Eldritch Gate" [Expedition, Desert, Ruins] Squiggle [Hourglass, T, Heart] TheNightsUsurper

expeditionCampGuardiansOfTheAbyss :: CardDef
expeditionCampGuardiansOfTheAbyss =
  singleSided
    $ location
      "83037"
      "Expedition Camp"
      [Expedition, Cairo, Desert]
      Circle
      [Diamond, Hourglass, Moon, Plus]
      SandsOfEgypt

nileRiver :: CardDef
nileRiver =
  singleSided
    $ location "83038" "Nile River" [Expedition, Desert] Moon [Circle, Star, Heart, Hourglass] SandsOfEgypt

sandsOfDashur :: CardDef
sandsOfDashur =
  singleSided
    $ location
      "83039"
      "Sands of Dashur"
      [Expedition, Desert]
      Hourglass
      [Circle, Plus, T, Squiggle, Heart, Moon]
      SandsOfEgypt

dunesOfTheSahara :: CardDef
dunesOfTheSahara =
  singleSided
    $ location
      "83040"
      "Dunes of the Sahara"
      [Expedition, Desert]
      Plus
      [Circle, Hourglass, T, Droplet]
      SandsOfEgypt

untouchedVault :: CardDef
untouchedVault =
  singleSided
    $ location "83041" "Untouched Vault" [Expedition, Ruins] Droplet [Plus, T] SandsOfEgypt

facelessSphinx :: CardDef
facelessSphinx =
  singleSided
    $ location "83042" "Faceless Sphinx" [Expedition, Desert, Ruins] Star [Moon, Heart] SandsOfEgypt

desertOasis :: CardDef
desertOasis =
  singleSided
    $ location "83043" "Desert Oasis" [Expedition, Desert] Heart [Star, Moon, Hourglass, Squiggle] SandsOfEgypt

sandsweptRuins :: CardDef
sandsweptRuins =
  singleSided
    $ location
      "83044"
      "Sandswept Ruins"
      [Expedition, Desert, Ruins]
      T
      [Droplet, Plus, Hourglass, Squiggle]
      SandsOfEgypt

arkham :: CardDef
arkham =
  location
    "86014"
    "Arkham"
    [RitualSite, Town]
    Triangle
    [Star, Hourglass, Circle]
    WarOfTheOuterGods

streetsOfProvidence :: CardDef
streetsOfProvidence =
  victory 1
    $ location
      "86015"
      "Streets of Providence"
      [Providence]
      Star
      [Triangle, T, Droplet]
      WarOfTheOuterGods

athenaeumOfTheEmptySky :: CardDef
athenaeumOfTheEmptySky =
  location
    "86016"
    "Athenaeum of the Empty Sky"
    [RitualSite, Providence]
    Droplet
    [Star, T]
    WarOfTheOuterGods

theArcade :: CardDef
theArcade = location "86017" "The Arcade" [Providence] T [Star, Droplet] WarOfTheOuterGods

streetsOfMontreal :: CardDef
streetsOfMontreal =
  victory 1
    $ location
      "86018"
      "Streets of Montréal"
      [Montreal]
      Hourglass
      [Triangle, Equals, Heart]
      WarOfTheOuterGods

chateauRamezay :: CardDef
chateauRamezay =
  location "86019" "Chateau Ramezay" [Montreal] Equals [Hourglass, Heart] WarOfTheOuterGods

shrineOfMaghanArkat :: CardDef
shrineOfMaghanArkat =
  location
    "86020"
    "Shrine of Magh'an Ark'at"
    [RitualSite, Montreal]
    Heart
    [Hourglass, Equals]
    WarOfTheOuterGods

streetsOfNewYorkCity :: CardDef
streetsOfNewYorkCity =
  victory 1
    $ location
      "86021"
      "Streets of New York City"
      [NewYorkCity]
      Circle
      [Triangle, Moon, Spade]
      WarOfTheOuterGods

theBurningPit :: CardDef
theBurningPit =
  location "86022" "The Burning Pit" [RitualSite, NewYorkCity] Moon [Circle, Spade] WarOfTheOuterGods

thePenthouse :: CardDef
thePenthouse =
  location "86023" "The Penthouse" [NewYorkCity] Spade [Circle, Moon] WarOfTheOuterGods

hubDimension :: CardDef
hubDimension =
  (location "86024" "Hub Dimension" [Portal] NoSymbol [] WarOfTheOuterGods)
    { cdRevealedName = Just ("Hub Dimension" <:> "Gateway to Destruction")
    }

tindalos :: CardDef
tindalos =
  (location
     "87005a"
     ("Tindalos" <:> "Realm of Angular Time")
     [Past, Present, Future]
     NoSymbol
     []
     MachinationsThroughTimeSingleGroup
  )
    { cdArt = "87005"
    }

arkhamGazette :: CardDef
arkhamGazette =
  location "87007" "Arkham Gazette" [Arkham, Portal, Past] Star [Hourglass] MachinationsThroughTime

oMalleysWatchShop :: CardDef
oMalleysWatchShop =
  victory 1
    $ location
      "87008"
      "O'Malley's Watch Shop"
      [Arkham, Portal, Past]
      Hourglass
      [Star, Triangle]
      MachinationsThroughTime

riverDocksPast :: CardDef
riverDocksPast =
  location "87009" "River Docks" [Arkham, Portal, Past] Hourglass [Star, Triangle] MachinationsThroughTime

miskatonicUniversityPast :: CardDef
miskatonicUniversityPast =
  location
    "87010"
    "Miskatonic University"
    [Arkham, Portal, Past]
    Triangle
    [Hourglass, Heart]
    MachinationsThroughTime

childhoodHome :: CardDef
childhoodHome =
  location "87011" "Childhood Home" [Arkham, Past] Heart [Triangle] MachinationsThroughTime

arkhamAdvertiserPresent :: CardDef
arkhamAdvertiserPresent =
  location "87016" "Arkham Advertiser" [Arkham, Portal, Present] Equals [Trefoil] MachinationsThroughTime

tickTockClubPresent :: CardDef
tickTockClubPresent =
  victory 1
    $ location
      "87017"
      "Tick-Tock Club"
      [Arkham, Portal, Present]
      Trefoil
      [Equals, Circle]
      MachinationsThroughTime

riverDocksPresent :: CardDef
riverDocksPresent =
  location
    "87018"
    "River Docks"
    [Arkham, Portal, Present]
    Trefoil
    [Equals, Circle]
    MachinationsThroughTime

miskatonicUniversityPresent :: CardDef
miskatonicUniversityPresent =
  location
    "87019"
    "Miskatonic University"
    [Arkham, Portal, Present]
    Circle
    [Trefoil, Moon]
    MachinationsThroughTime

yeOldeMagickShoppe :: CardDef
yeOldeMagickShoppe =
  location "87020" "Ye Olde Magick Shoppe" [Arkham, Present] Moon [Circle] MachinationsThroughTime

arkhamAdvertiserFuture :: CardDef
arkhamAdvertiserFuture =
  location "87025" "Arkham Advertiser" [Arkham, Portal, Future] Plus [Spade] MachinationsThroughTime

tickTockClubFuture :: CardDef
tickTockClubFuture =
  victory 1
    $ location
      "87026"
      "Tick-Tock Club"
      [Arkham, Portal, Future]
      Spade
      [Plus, Diamond]
      MachinationsThroughTime

riverDocksFuture :: CardDef
riverDocksFuture =
  location "87027" "River Docks" [Arkham, Portal, Future] Spade [Plus, Diamond] MachinationsThroughTime

miskatonicUniversityFuture :: CardDef
miskatonicUniversityFuture =
  location
    "87028"
    "Miskatonic University"
    [Arkham, Portal, Future]
    Diamond
    [Spade, Squiggle]
    MachinationsThroughTime

corriganIndustries :: CardDef
corriganIndustries =
  location "87029" "Corrigan Industries" [Arkham, Future] Squiggle [Diamond] MachinationsThroughTime

chamberOfSecretsBloodyPrison :: CardDef
chamberOfSecretsBloodyPrison =
  location
    "70016"
    ("Chamber of Secrets" <:> "Bloody Prison")
    [Prison, Distortion]
    Circle
    [Equals]
    TheLabyrinthsOfLunacy

chamberOfSecretsMysteriousPrison :: CardDef
chamberOfSecretsMysteriousPrison =
  location
    "70017"
    ("Chamber of Secrets" <:> "Mysterious Prison")
    [Prison, Distortion]
    Circle
    [Equals]
    TheLabyrinthsOfLunacy

chamberOfSecretsEnshroudedPrison :: CardDef
chamberOfSecretsEnshroudedPrison =
  location
    "70018"
    ("Chamber of Secrets" <:> "Enshrouded Prison")
    [Prison, Distortion]
    Circle
    [Equals]
    TheLabyrinthsOfLunacy

chamberOfRain :: CardDef
chamberOfRain =
  location "70019" "Chamber of Rain" [Distortion] Hourglass [Heart] TheLabyrinthsOfLunacy

chamberOfSorrowsEpicMultiplayer :: CardDef
chamberOfSorrowsEpicMultiplayer =
  location
    "70020"
    "Chamber of Sorrows"
    [Prison]
    Heart
    [Hourglass, Equals]
    LabyrinthsOfLunacyEpicMultiplayer

chamberOfSorrows :: CardDef
chamberOfSorrows =
  location
    "70021"
    "Chamber of Sorrows"
    [Prison]
    Heart
    [Hourglass, Equals]
    LabyrinthsOfLunacySingleGroup

chamberOfNightEpicMultiplayer :: CardDef
chamberOfNightEpicMultiplayer =
  location
    "70022"
    "Chamber of Night"
    [Prison]
    Square
    [Triangle, Equals]
    LabyrinthsOfLunacyEpicMultiplayer

chamberOfNight :: CardDef
chamberOfNight =
  location
    "70023"
    "Chamber of Night"
    [Prison]
    Square
    [Triangle, Equals]
    LabyrinthsOfLunacySingleGroup

chamberOfRegret :: CardDef
chamberOfRegret =
  location "70024" "Chamber of Regret" [Distortion] Triangle [Square] TheLabyrinthsOfLunacy

labyrinthineHallsFoulSmellingPath :: CardDef
labyrinthineHallsFoulSmellingPath =
  locationWithUnrevealed
    "70025"
    "Labyrinthine Halls"
    []
    Equals
    [Circle, Square, Heart, Equals]
    ("Labyrinthine Halls" <:> "Foul-smelling Path")
    []
    Equals
    [Circle, Square, Heart, Equals, Diamond]
    TheLabyrinthsOfLunacy

labyrinthineHallsCorpseFilledPath :: CardDef
labyrinthineHallsCorpseFilledPath =
  locationWithUnrevealed
    "70026"
    "Labyrinthine Halls"
    []
    Equals
    [Circle, Square, Heart, Equals]
    ("Labyrinthine Halls" <:> "Corpse-filled Path")
    []
    Equals
    [Circle, Square, Heart, Equals, Star]
    TheLabyrinthsOfLunacy

labyrinthineHallsOvergrownPath :: CardDef
labyrinthineHallsOvergrownPath =
  locationWithUnrevealed
    "70027"
    "Labyrinthine Halls"
    []
    Equals
    [Circle, Square, Heart, Equals]
    ("Labyrinthine Halls" <:> "Overgrown Path")
    []
    Equals
    [Circle, Square, Heart, Equals, Moon]
    TheLabyrinthsOfLunacy

chamberOfHunger :: CardDef
chamberOfHunger =
  location "70028" "Chamber of Hunger" [Distortion] Star [Equals, T] TheLabyrinthsOfLunacy

chamberOfDecay :: CardDef
chamberOfDecay =
  location "70029" "Chamber of Decay" [Distortion] Moon [Equals, T] TheLabyrinthsOfLunacy

chamberOfRot :: CardDef
chamberOfRot =
  location "70030" "Chamber of Rot" [Distortion] Diamond [Equals, Squiggle, T] TheLabyrinthsOfLunacy

chamberOfPoison :: CardDef
chamberOfPoison =
  location "70031" "Chamber of Poison" [] Squiggle [Diamond] TheLabyrinthsOfLunacy

abandonedWarehouse :: CardDef
abandonedWarehouse =
  location
    "70032"
    "Abandoned Warehouse"
    [Distortion]
    T
    [Diamond, Moon, Star]
    TheLabyrinthsOfLunacy
