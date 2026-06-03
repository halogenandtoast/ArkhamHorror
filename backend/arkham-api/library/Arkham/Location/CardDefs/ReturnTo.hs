module Arkham.Location.CardDefs.ReturnTo where

import Arkham.Location.CardDefs.Import

studyAberrantGateway :: CardDef
studyAberrantGateway =
  location
    "50013"
    ("Study" <:> "Aberrant Gateway")
    mempty
    Circle
    [T]
    ReturnToTheGathering

guestHall :: CardDef
guestHall =
  location
    "50014"
    "Guest Hall"
    mempty
    T
    [Circle, Heart, Star, Square]
    ReturnToTheGathering

bedroom :: CardDef
bedroom = location "50015" "Bedroom" mempty Heart [T] ReturnToTheGathering

bathroom :: CardDef
bathroom = location "50016" "Bathroom" mempty Star [T] ReturnToTheGathering

holeInTheWall :: CardDef
holeInTheWall =
  locationWithUnrevealed
    "50017"
    "Hole in the Wall"
    mempty
    Square
    [T]
    "Hallway"
    mempty
    Square
    [T, Triangle, Plus, Diamond]
    ReturnToTheGathering

returnToAttic :: CardDef
returnToAttic =
  locationWithUnrevealed
    "50018"
    "Attic"
    mempty
    Triangle
    [Square]
    "Attic"
    mempty
    Triangle
    [Square, Moon]
    ReturnToTheGathering

farAboveYourHouse :: CardDef
farAboveYourHouse =
  victory 1
    $ locationWithUnrevealed
      "50019"
      "Far Above Your House"
      mempty
      Moon
      [Triangle]
      "Field of Graves"
      mempty
      Moon
      [Triangle]
      ReturnToTheGathering

returnToCellar :: CardDef
returnToCellar =
  locationWithUnrevealed
    "50020"
    "Cellar"
    mempty
    Plus
    [Square]
    "Cellar"
    mempty
    Plus
    [Square, Squiggle]
    ReturnToTheGathering

deepBelowYourHouse :: CardDef
deepBelowYourHouse =
  victory 1
    $ locationWithUnrevealed
      "50021"
      "Deep Below Your House"
      mempty
      Squiggle
      [Plus]
      "Ghoul Pits"
      mempty
      Squiggle
      [Plus]
      ReturnToTheGathering

easttownArkhamPoliceStation :: CardDef
easttownArkhamPoliceStation =
  victory 1
    $ locationWithUnrevealed
      "50027"
      "Easttown"
      [Arkham]
      Moon
      [Circle, Triangle]
      ("Easttown" <:> "Arkham Police Station")
      [Arkham]
      Moon
      [Circle, Triangle]
      ReturnToTheMidnightMasks

northsideTrainStation :: CardDef
northsideTrainStation =
  locationWithUnrevealed
    "50028"
    ("Northside" <:> "Train Station")
    [Arkham]
    T
    [Diamond, Triangle]
    ("Northside" <:> "Train Station")
    [Arkham]
    T
    [Diamond, Triangle]
    ReturnToTheMidnightMasks

miskatonicUniversityMiskatonicMuseum :: CardDef
miskatonicUniversityMiskatonicMuseum =
  locationWithUnrevealed
    "50029"
    "Miskatonic University"
    [Arkham]
    Diamond
    [T, Plus, Circle, Square]
    ("Miskatonic University" <:> "Miskatonic Museum")
    [Arkham]
    Diamond
    [T, Plus, Circle, Square]
    ReturnToTheMidnightMasks

rivertownAbandonedWarehouse :: CardDef
rivertownAbandonedWarehouse =
  locationWithUnrevealed
    "50030"
    "Rivertown"
    [Arkham, Central]
    Circle
    [Moon, Diamond, Square, Squiggle, Hourglass]
    ("Rivertown" <:> "Abandoned Warehouse")
    [Arkham, Central]
    Circle
    [Moon, Diamond, Square, Squiggle, Hourglass]
    ReturnToTheMidnightMasks

arkhamWoodsGreatWillow :: CardDef
arkhamWoodsGreatWillow =
  locationWithUnrevealed
    "50033"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Great Willow")
    [Woods]
    Heart
    [Squiggle, Star]
    ReturnToTheDevourerBelow

arkhamWoodsLakeside :: CardDef
arkhamWoodsLakeside =
  locationWithUnrevealed
    "50034"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Lakeside")
    [Woods]
    Star
    [Squiggle, Heart]
    ReturnToTheDevourerBelow

arkhamWoodsCorpseRiddenClearing :: CardDef
arkhamWoodsCorpseRiddenClearing =
  locationWithUnrevealed
    "50035"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Corpse-Ridden Clearing")
    [Woods]
    Droplet
    [Squiggle, Circle]
    ReturnToTheDevourerBelow

arkhamWoodsWoodenBridge :: CardDef
arkhamWoodsWoodenBridge =
  locationWithUnrevealed
    "50036"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Wooden Bridge")
    [Woods]
    Circle
    [Squiggle, Droplet]
    ReturnToTheDevourerBelow

warrenObservatory :: CardDef
warrenObservatory =
  victory 1
    $ location
      "51013"
      "Warren Observatory"
      [Miskatonic]
      Triangle
      [Plus, Square]
      ReturnToExtracurricularActivities

returnToCloverClubLounge :: CardDef
returnToCloverClubLounge =
  locationWithUnrevealed
    "51016"
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle]
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle, Heart]
    ReturnToTheHouseAlwaysWins

cloverClubStage :: CardDef
cloverClubStage =
  location
    "51017"
    "Clover Club Stage"
    [CloverClub]
    Heart
    [Circle, Square]
    ReturnToTheHouseAlwaysWins

exhibitHallMedievalExhibit :: CardDef
exhibitHallMedievalExhibit =
  victory 1
    $ locationWithUnrevealed
      "51021"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Medieval Exhibit")
      [Miskatonic, Exhibit]
      Star
      [Square]
      ReturnToTheHouseAlwaysWins

exhibitHallTheArchives :: CardDef
exhibitHallTheArchives =
  locationWithUnrevealed
    "51022"
    "Exhibit Hall"
    [Miskatonic, Exhibit]
    NoSymbol
    [Square]
    ("Exhibit Hall" <:> "The Archives")
    [Miskatonic, Exhibit]
    Hourglass
    [Square, Equals]
    ReturnToTheHouseAlwaysWins

returnToEngineCar :: CardDef
returnToEngineCar =
  victory 1 $ location "51028" "Engine Car" [Train] NoSymbol [] ReturnToTheEssexCountyExpress

freightCar :: CardDef
freightCar =
  locationWithUnrevealed_
    "51029"
    "Train Car"
    [Train]
    "Freight Car"
    [Train]
    ReturnToTheEssexCountyExpress

baggageCar :: CardDef
baggageCar =
  locationWithUnrevealed_
    "51030"
    "Train Car"
    [Train]
    "Baggage Car"
    [Train]
    ReturnToTheEssexCountyExpress

villageCommonsSilentDecay :: CardDef
villageCommonsSilentDecay =
  location
    "51033"
    ("Village Commons" <:> "Silent Decay")
    [Dunwich, Central]
    Plus
    [Square, Diamond, Moon]
    ReturnToBloodOnTheAltar

returnToBishopsBrook :: CardDef
returnToBishopsBrook =
  location
    "51034"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    ReturnToBloodOnTheAltar

returnToBurnedRuins :: CardDef
returnToBurnedRuins =
  location
    "51035"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    ReturnToBloodOnTheAltar

returnToOsbornsGeneralStore :: CardDef
returnToOsbornsGeneralStore =
  location
    "51036"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    ReturnToBloodOnTheAltar

returnToCongregationalChurch :: CardDef
returnToCongregationalChurch =
  location
    "51037"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    ReturnToBloodOnTheAltar

returnToHouseInTheReeds :: CardDef
returnToHouseInTheReeds =
  location
    "51038"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    ReturnToBloodOnTheAltar

returnToSchoolhouse :: CardDef
returnToSchoolhouse =
  location
    "51039"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    ReturnToBloodOnTheAltar

baseOfTheHillWarpedAndTwisted :: CardDef
baseOfTheHillWarpedAndTwisted =
  location
    "51048"
    ("Base of the Hill" <:> "Warped and Twisted")
    [Dunwich, SentinelHill]
    Triangle
    [Square, Plus, Squiggle, Hourglass, Droplet]
    ReturnToWhereDoomAwaits

ascendingPathWarpedAndTwisted :: CardDef
ascendingPathWarpedAndTwisted =
  location
    "51049"
    ("Ascending Path" <:> "Warped and Twisted")
    [Dunwich, SentinelHill]
    Square
    [Triangle, Diamond, T, Equals, Moon, Trefoil]
    ReturnToWhereDoomAwaits

abandonedCamp :: CardDef
abandonedCamp =
  locationWithUnrevealed
    "51050"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Abandoned Camp"
    [Dunwich, Woods]
    Droplet
    [Triangle, Trefoil]
    ReturnToWhereDoomAwaits

fathomlessLake :: CardDef
fathomlessLake =
  locationWithUnrevealed
    "51051"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Fathomless Lake"
    [Dunwich, Woods, Altered]
    Trefoil
    [Square, Droplet]
    ReturnToWhereDoomAwaits

realmsBeyondAllInOne :: CardDef
realmsBeyondAllInOne =
  singleSided
    $ location
      "51057"
      ("Realms Beyond" <:> "All-In-One")
      [Otherworld]
      Droplet
      [Circle]
      ReturnToLostInTimeAndSpace

indecipherableStairs :: CardDef
indecipherableStairs =
  singleSided
    $ revelation
    $ location
      "51058"
      "Indecipherable Stairs"
      [Otherworld, Extradimensional]
      Triangle
      [Square, Equals]
      ReturnToLostInTimeAndSpace

toweringLuminosity :: CardDef
toweringLuminosity =
  singleSided
    $ revelation
    $ location
      "51059"
      "Towering Luminosity"
      [Otherworld, Extradimensional]
      Diamond
      [Square, Equals]
      ReturnToLostInTimeAndSpace

unstableVortex :: CardDef
unstableVortex =
  singleSided
    $ revelation
    $ location
      "51060"
      "Unstable Vortex"
      [Otherworld, Extradimensional]
      Equals
      [Square, Moon, Plus, Squiggle]
      ReturnToLostInTimeAndSpace

theatreLounge :: CardDef
theatreLounge =
  victory 1
    $ locationWithUnrevealed
      "52018"
      "Lobby Doorway"
      [Private]
      Plus
      [Triangle]
      "Theatre Lounge"
      [Private]
      Plus
      [Triangle]
      ReturnToCurtainCall

propShop :: CardDef
propShop =
  victory 1
    $ locationWithUnrevealed
      "52019"
      "Backstage Doorway"
      [Private]
      Moon
      [Diamond]
      "Prop Shop"
      [Private]
      Moon
      [Diamond]
      ReturnToCurtainCall

returnToQuietHalls :: CardDef
returnToQuietHalls = location "52029" "Quiet Halls" [Basement] Droplet [Square] ReturnToEchoesOfThePast

historicalSocietyDustyArchives :: CardDef
historicalSocietyDustyArchives =
  locationWithUnrevealed
    "52030"
    "Historical Society"
    [Basement]
    NoSymbol
    [Droplet]
    ("Historical Society" <:> "Dusty Archives")
    [Basement, Passageway]
    Trefoil
    [Droplet]
    ReturnToEchoesOfThePast

historicalSocietyMuseumStorage :: CardDef
historicalSocietyMuseumStorage =
  locationWithUnrevealed
    "52031"
    "Historical Society"
    [Basement]
    NoSymbol
    [Droplet]
    ("Historical Society" <:> "Museum Storage")
    [Basement, Passageway]
    Trefoil
    [Droplet]
    ReturnToEchoesOfThePast

historicalSocietyBoilerRoom :: CardDef
historicalSocietyBoilerRoom =
  locationWithUnrevealed
    "52032"
    "Historical Society"
    [Basement]
    NoSymbol
    [Droplet]
    ("Historical Society" <:> "Boiler Room")
    [Basement]
    Trefoil
    [Droplet]
    ReturnToEchoesOfThePast

returnToMontparnasse :: CardDef
returnToMontparnasse =
  location "52041" "Montparnasse" [Paris, Rail] Circle [Heart, Star, Plus] ReturnToAPhantomOfTruth

returnToGrandGuignol :: CardDef
returnToGrandGuignol =
  victory 1
    $ location
      "52042"
      ("Grand Guignol" <:> "Theatre of the Great Puppet")
      [Paris]
      Triangle
      [Diamond, Square]
      ReturnToAPhantomOfTruth

returnToPereLachaiseCemetery :: CardDef
returnToPereLachaiseCemetery =
  victory 1
    $ location "52043" "Père Lachaise Cemetery" [Paris] T [Equals, Moon] ReturnToAPhantomOfTruth

returnToCanalSaintMartin :: CardDef
returnToCanalSaintMartin =
  victory 1
    $ location "52044" "Canal Saint-Martin" [Paris] Equals [Square, T, Moon] ReturnToAPhantomOfTruth

returnToNotreDame :: CardDef
returnToNotreDame = location "52045" "Notre-Dame" [Paris, Rail] Plus [Circle, Moon, Star] ReturnToAPhantomOfTruth

returnToGardensOfLuxembourg :: CardDef
returnToGardensOfLuxembourg =
  victory 1
    $ location "52046" "Gardens of Luxembourg" [Paris] Star [Circle, Heart, Plus] ReturnToAPhantomOfTruth

returnToSecretPassage :: CardDef
returnToSecretPassage =
  locationWithUnrevealed_ "52049" "Catacombs" [] "Secret Passage" [] ReturnToThePallidMask

moundOfBones :: CardDef
moundOfBones =
  locationWithUnrevealed_ "52050" "Catacombs" [] "Mound of Bones" [] ReturnToThePallidMask

researchSite :: CardDef
researchSite =
  locationWithUnrevealed_ "52051" "Catacombs" [] "Research Site" [] ReturnToThePallidMask

seaOfSkulls :: CardDef
seaOfSkulls =
  locationWithUnrevealed_ "52052" "Catacombs" [] "Sea of Skulls" [] ReturnToThePallidMask

returnToCloister :: CardDef
returnToCloister = location "52055" "Cloister" [] Heart [Square, Hourglass] ReturnToBlackStarsRise

returnToKnightsHall :: CardDef
returnToKnightsHall = location "52056" "Knight's Hall" [] Hourglass [Square, Heart] ReturnToBlackStarsRise

returnToPalaceOfTheKing :: CardDef
returnToPalaceOfTheKing =
  storyOnBack
    $ location
      "52060"
      ("Palace of the King" <:> "Hastur's Domain")
      [Otherworld]
      Star
      [Triangle, Diamond, Heart, Droplet, Hourglass]
      ReturnToDimCarcosa

recessesOfYourOwnMind :: CardDef
recessesOfYourOwnMind = storyOnBack $ location "52061" "Recesses of Your Own Mind" [] Heart [Star] ReturnToDimCarcosa

theThroneRoom :: CardDef
theThroneRoom = storyOnBack $ location "52062" "The Throne Room" [] Droplet [Star] ReturnToDimCarcosa

stageOfTheWardTheatre :: CardDef
stageOfTheWardTheatre = storyOnBack $ location "52063" "Stage of the Ward Theatre" [] Hourglass [Star] ReturnToDimCarcosa

entrywayRearrangedByTime :: CardDef
entrywayRearrangedByTime =
  location
    "53019"
    ("Entryway" <:> "Rearranged by Time")
    [Ancient, Ruins]
    Circle
    [Square, Diamond, Star]
    ReturnToTheDoomOfEztli

sealedPassage :: CardDef
sealedPassage =
  singleSided
    $ location
      "53020"
      "Sealed Passage"
      [Ancient, Ruins]
      Diamond
      [Circle, Square, Star]
      ReturnToTheDoomOfEztli

mosaicChamber :: CardDef
mosaicChamber =
  singleSided
    $ location
      "53021"
      "Mosaic Chamber"
      [Ancient, Ruins]
      Heart
      [Square, Plus, T, Triangle]
      ReturnToTheDoomOfEztli

tombOfTheAncients :: CardDef
tombOfTheAncients =
  singleSided
    $ location
      "53022"
      "Tomb of the Ancients"
      [Ancient, Ruins]
      Triangle
      [Heart, Plus, T, Hourglass]
      ReturnToTheDoomOfEztli

throneRoom :: CardDef
throneRoom =
  victory 1
    $ vengeance 1
    $ singleSided
    $ location
      "53023"
      "Throne Room"
      [Ancient, Ruins]
      Plus
      [Star, Heart, T, Triangle]
      ReturnToTheDoomOfEztli

snakePit :: CardDef
snakePit =
  singleSided
    $ location
      "53024"
      "Snake Pit"
      [Ancient, Ruins]
      T
      [Heart, Plus, Triangle, Hourglass]
      ReturnToTheDoomOfEztli

ancientHallRearrangedByTime :: CardDef
ancientHallRearrangedByTime =
  singleSided
    $ location
      "53025"
      ("Ancient Hall" <:> "Rearranged by Time")
      [Ancient, Ruins]
      Square
      [Circle, Diamond, Heart]
      ReturnToTheDoomOfEztli

grandChamberRearrangedByTime :: CardDef
grandChamberRearrangedByTime =
  vengeance 1
    $ singleSided
    $ location
      "53026"
      ("Grand Chamber" <:> "Rearranged by Time")
      [Ancient, Ruins]
      Star
      [Circle, Diamond, Plus]
      ReturnToTheDoomOfEztli

chamberOfTimeRearrangedByTime :: CardDef
chamberOfTimeRearrangedByTime =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "53027"
      ("Chamber of Time" <:> "Rearranged by Time")
      [Forgotten, Ruins]
      Hourglass
      [Squiggle, Triangle, T]
      ReturnToTheDoomOfEztli

theHastingsEstate :: CardDef
theHastingsEstate =
  victory 1
    $ otherSideIs "53029"
    $ location
      "53029b"
      "The Hastings Estate"
      [Arkham]
      Square
      [Diamond, Circle]
      ReturnToThreadsOfFate

loadingDocks :: CardDef
loadingDocks =
  victory 1
    $ otherSideIs "53034"
    $ location
      "53034b"
      "Loading Docks"
      [Arkham]
      Squiggle
      [Circle]
      ReturnToThreadsOfFate

returnToTempleRuins :: CardDef
returnToTempleRuins =
  location
    "53039"
    "Temple Ruins"
    [MexicoCity, PresentDay]
    Circle
    [Diamond, Star]
    ReturnToTheBoundaryBeyond

returnToMetropolitanCathedral :: CardDef
returnToMetropolitanCathedral =
  location
    "53040"
    "Metropolitan Cathedral"
    [MexicoCity, PresentDay]
    Square
    [Diamond]
    ReturnToTheBoundaryBeyond

returnToChapultepecPark :: CardDef
returnToChapultepecPark =
  location
    "53041"
    "Chapultepec Park"
    [MexicoCity, PresentDay]
    Triangle
    [Star]
    ReturnToTheBoundaryBeyond

returnToZocalo :: CardDef
returnToZocalo =
  location
    "53042"
    "Zócalo"
    [MexicoCity, PresentDay]
    Diamond
    [Heart, Square, Star, Circle]
    ReturnToTheBoundaryBeyond

returnToXochimilco :: CardDef
returnToXochimilco =
  location
    "53043"
    "Xochimilco"
    [MexicoCity, PresentDay]
    Heart
    [Diamond, Star]
    ReturnToTheBoundaryBeyond

returnToCoyoacan :: CardDef
returnToCoyoacan =
  location
    "53044"
    "Coyoacán"
    [MexicoCity, PresentDay]
    Star
    [Diamond, Triangle, Circle, Heart]
    ReturnToTheBoundaryBeyond

ruinsOfKnYan :: CardDef
ruinsOfKnYan =
  victory 1
    $ singleSided
    $ location
      "53049"
      "Ruins of K'n-Yan"
      [Ancient, Cave, Ruins]
      Triangle
      [Equals, Circle, Square]
      ReturnToKnYan

subterraneanSwamp :: CardDef
subterraneanSwamp =
  victory 1
    $ singleSided
    $ location
      "53050"
      "Subterranean Swamp"
      [Ancient, Cave]
      Diamond
      [Equals, Circle, Moon]
      ReturnToKnYan

chthonianDepths :: CardDef
chthonianDepths =
  victory 1
    $ singleSided
    $ location
      "53051"
      "Chthonian Depths"
      [Ancient, Cave]
      Square
      [Heart, Triangle, Circle]
      ReturnToKnYan

treacherousDescent :: CardDef
treacherousDescent =
  victory 1
    $ singleSided
    $ location
      "53052"
      "Treacherous Descent"
      [Ancient, Cave]
      Moon
      [Heart, Diamond, Circle]
      ReturnToKnYan

hallsOfPnakotusSouthernCorridors :: CardDef
hallsOfPnakotusSouthernCorridors =
  location
    "53055"
    ("Halls of Pnakotus" <:> "Southern Corridors")
    [Ancient, Pnakotus]
    Plus
    [Square, Diamond, Heart, Hourglass]
    ReturnToTheCityOfArchives

cyclopeanVaults :: CardDef
cyclopeanVaults =
  victory 1
    $ location
      "53056"
      "Cyclopean Vaults"
      [Ancient, Pnakotus]
      Hourglass
      [Plus, Heart]
      ReturnToTheCityOfArchives

alienConservatory :: CardDef
alienConservatory =
  location
    "53057"
    "Alien Conservatory"
    [Ancient, Pnakotus]
    Heart
    [Plus, Hourglass]
    ReturnToTheCityOfArchives

greatHallOfCeleano :: CardDef
greatHallOfCeleano =
  singleSided
    $ location "53062" "Great Hall of Celeano" [Otherworld] Droplet [Diamond] ReturnToShatteredAeons

buenosAires :: CardDef
buenosAires =
  singleSided
    $ location "53063" "Buenos Aires" [Shattered] Equals [Star] ReturnToShatteredAeons

ultimaThule :: CardDef
ultimaThule =
  singleSided
    $ location "53064" "Ultima Thule" [Shattered] Equals [Star] ReturnToShatteredAeons

riversideTemple :: CardDef
riversideTemple =
  singleSided
    $ location
      "53067"
      "Riverside Temple"
      [Ancient, Jungle]
      Square
      [Circle, Diamond, Triangle, Squiggle]
      ReturnToRainforest

waterfall :: CardDef
waterfall =
  singleSided
    $ location
      "53068"
      "Waterfall"
      [Jungle]
      Moon
      [Circle, Diamond, Heart, T]
      ReturnToRainforest

trailOfTheDead :: CardDef
trailOfTheDead =
  victory 1
    $ singleSided
    $ location
      "53069"
      "Trail of the Dead"
      [Jungle]
      Triangle
      [Squiggle, Square, Diamond, Hourglass]
      ReturnToRainforest

cloudForest :: CardDef
cloudForest =
  victory 1
    $ singleSided
    $ location
      "53070"
      "Cloud Forest"
      [Jungle]
      Heart
      [Hourglass, Diamond, Moon, T]
      ReturnToRainforest

witchHauntedWoodsWitchTree :: CardDef
witchHauntedWoodsWitchTree =
  victory 1
    $ locationWithUnrevealed
      "54019"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Witch Tree")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ReturnToTheWitchingHour

witchHauntedWoodsUnmarkedGraveyard :: CardDef
witchHauntedWoodsUnmarkedGraveyard =
  victory 1
    $ locationWithUnrevealed
      "54020"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Unmarked Graveyard")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ReturnToTheWitchingHour

arkhamWoodsHiddenPath :: CardDef
arkhamWoodsHiddenPath =
  locationWithUnrevealed
    "54021"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Hidden Path")
    [Woods]
    Spade
    [Squiggle, Trefoil]
    ReturnToTheWitchingHour

arkhamWoodsPlaceOfPower :: CardDef
arkhamWoodsPlaceOfPower =
  locationWithUnrevealed
    "54022"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Place of Power")
    [Woods]
    Trefoil
    [Squiggle, Spade]
    ReturnToTheWitchingHour

arkhamWoodsBootleggingOperation :: CardDef
arkhamWoodsBootleggingOperation =
  locationWithUnrevealed
    "54023"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Bootlegging Operation")
    [Woods]
    Trefoil
    [Squiggle, Equals, Hourglass]
    ReturnToTheWitchingHour

wineCellar :: CardDef
wineCellar =
  location
    "54027"
    "Wine Cellar"
    []
    Hourglass
    [T]
    ReturnToAtDeathsDoorstep

wineCellarSpectral :: CardDef
wineCellarSpectral =
  location
    "54028"
    "Wine Cellar"
    [Spectral]
    Hourglass
    [T]
    ReturnToAtDeathsDoorstep

templeOfRlyeh :: CardDef
templeOfRlyeh =
  victory 1
    $ locationWithUnrevealed
      "54030"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Temple of R'lyeh"
      [Extradimensional, Otherworld]
      Equals
      [Square, Squiggle]
      ReturnToTheSecretName

thePriceManor :: CardDef
thePriceManor =
  locationWithUnrevealed
    "54031"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "The Price Manor"
    [Extradimensional]
    Moon
    [Square]
    ReturnToTheSecretName

the9thWard :: CardDef
the9thWard =
  locationWithUnrevealed
    "54032"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "The 9th Ward"
    [Extradimensional]
    Moon
    [Square]
    ReturnToTheSecretName

libraryOfEbla :: CardDef
libraryOfEbla =
  victory 1
    $ locationWithUnrevealed
      "54033"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Library of Ebla"
      [Extradimensional]
      Squiggle
      [Square, Equals]
      ReturnToTheSecretName

returnToHangmansBrook :: CardDef
returnToHangmansBrook =
  otherSideIs "54037b"
    $ location "54037" "Hangman's Brook" mempty Squiggle [Circle, Plus] ReturnToTheWagesOfSin

returnToHangmansBrookSpectral :: CardDef
returnToHangmansBrookSpectral =
  otherSideIs "54037"
    $ location
      "54037b"
      "Hangman's Brook"
      [Spectral]
      Squiggle
      [Circle, Plus]
      ReturnToTheWagesOfSin

returnToLounge :: CardDef
returnToLounge =
  locationWithUnrevealed
    "54043"
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus]
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus, Trefoil]
    ForTheGreaterGood

relicStorage :: CardDef
relicStorage =
  locationWithUnrevealed
    "54044"
    "Hidden Passageway"
    [Lodge]
    Trefoil
    [Moon]
    "Relic Storage"
    [Lodge]
    Trefoil
    [Moon]
    ReturnToForTheGreaterGood

shroudedArchive :: CardDef
shroudedArchive =
  locationWithUnrevealed
    "54045"
    "Sanctum Doorway"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Shrouded Archive"
    [Lodge, Sanctum]
    Triangle
    [Squiggle]
    ReturnToForTheGreaterGood

returnToFrenchHill :: CardDef
returnToFrenchHill =
  location
    "54050"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    ReturnToInTheClutchesOfChaos

returnToRivertown :: CardDef
returnToRivertown =
  location
    "54051"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    ReturnToInTheClutchesOfChaos

returnToSouthside :: CardDef
returnToSouthside =
  location
    "54052"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    ReturnToInTheClutchesOfChaos

returnToUptown :: CardDef
returnToUptown =
  location
    "54053"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    ReturnToInTheClutchesOfChaos

returnToSouthChurch :: CardDef
returnToSouthChurch =
  location
    "54054"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    ReturnToInTheClutchesOfChaos

returnToMerchantDistrict :: CardDef
returnToMerchantDistrict =
  location
    "54055"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    ReturnToInTheClutchesOfChaos

nightmareBreach :: CardDef
nightmareBreach =
  locationWithUnrevealed
    "54058"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Nightmare Breach"
    [Otherworld, Void]
    NoSymbol
    []
    ReturnToBeforeTheBlackThrone

interstellarAbyss :: CardDef
interstellarAbyss =
  locationWithUnrevealed
    "54059"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Interstellar Abyss"
    [Otherworld, Void]
    NoSymbol
    []
    ReturnToBeforeTheBlackThrone

windingGulf :: CardDef
windingGulf =
  locationWithUnrevealed
    "54060"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Interstellar Abyss"
    [Otherworld, Void]
    NoSymbol
    []
    ReturnToBeforeTheBlackThrone
