{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDunwichLegacy where

import Arkham.Location.CardDefs.Import
import Arkham.Keyword qualified as Keyword

miskatonicQuad :: CardDef
miskatonicQuad =
  location
    "02048"
    "Miskatonic Quad"
    [Miskatonic]
    Plus
    [Triangle, Hourglass, Square, Diamond, Circle]
    ExtracurricularActivity

humanitiesBuilding :: CardDef
humanitiesBuilding =
  location
    "02049"
    "Humanities Building"
    [Miskatonic]
    Square
    [Plus, Triangle]
    ExtracurricularActivity

orneLibrary :: CardDef
orneLibrary =
  victory 1
    $ location
      "02050"
      "Orne Library"
      [Miskatonic]
      Triangle
      [Plus, Square]
      ExtracurricularActivity

studentUnion :: CardDef
studentUnion =
  location
    "02051"
    "Student Union"
    [Miskatonic]
    Diamond
    [Plus, Equals]
    ExtracurricularActivity

dormitories :: CardDef
dormitories =
  victory 1
    $ location
      "02052"
      "Dormitories"
      [Miskatonic]
      Equals
      [Diamond]
      ExtracurricularActivity

administrationBuilding :: CardDef
administrationBuilding =
  location
    "02053"
    "Administration Building"
    [Miskatonic]
    Circle
    [Plus, T]
    ExtracurricularActivity

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung =
  victory 1
    $ location
      "02054"
      ("Faculty Offices" <:> "The Night is Still Young")
      [Miskatonic]
      T
      [Circle]
      ExtracurricularActivity

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate =
  location
    "02055"
    ("Faculty Offices" <:> "The Hour is Late")
    [Miskatonic]
    T
    [Circle]
    ExtracurricularActivity

scienceBuilding :: CardDef
scienceBuilding =
  location
    "02056"
    "Science Building"
    [Miskatonic]
    Hourglass
    [Plus, Squiggle]
    ExtracurricularActivity

alchemyLabs :: CardDef
alchemyLabs =
  location
    "02057"
    "Alchemy Labs"
    [Miskatonic]
    Squiggle
    [Hourglass]
    ExtracurricularActivity

laBellaLuna :: CardDef
laBellaLuna =
  location "02070" "La Bella Luna" [Arkham] Moon [Circle] TheHouseAlwaysWins

cloverClubLounge :: CardDef
cloverClubLounge =
  location
    "02071"
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle]
    TheHouseAlwaysWins

cloverClubBar :: CardDef
cloverClubBar =
  location
    "02072"
    "Clover Club Bar"
    [CloverClub]
    Square
    [Triangle, Circle]
    TheHouseAlwaysWins

cloverClubCardroom :: CardDef
cloverClubCardroom =
  location
    "02073"
    "Clover Club Cardroom"
    [CloverClub]
    Triangle
    [Circle, Square, Diamond]
    TheHouseAlwaysWins

darkenedHall :: CardDef
darkenedHall =
  location
    "02074"
    "Darkened Hall"
    [CloverClub]
    Diamond
    [Triangle, T, Hourglass, Plus, Squiggle]
    TheHouseAlwaysWins

artGallery :: CardDef
artGallery =
  victory 1
    $ locationWithUnrevealed
      "02075"
      "Back Hall Doorway"
      [CloverClub]
      T
      [Diamond]
      "Art Gallery"
      [CloverClub]
      Hourglass
      [Diamond]
      TheHouseAlwaysWins

vipArea :: CardDef
vipArea =
  victory 1
    $ locationWithUnrevealed
      "02076"
      "Back Hall Doorway"
      [CloverClub]
      T
      [Diamond]
      "VIP Area"
      [CloverClub]
      Plus
      [Diamond]
      TheHouseAlwaysWins

backAlley :: CardDef
backAlley =
  victory 1
    $ locationWithUnrevealed
      "02077"
      "Back Hall Doorway"
      [CloverClub]
      T
      [Diamond]
      "Back Alley"
      [CloverClub]
      Squiggle
      [Diamond]
      TheHouseAlwaysWins

museumEntrance :: CardDef
museumEntrance =
  location
    "02126"
    "Museum Entrance"
    [Miskatonic]
    Circle
    [Square]
    TheMiskatonicMuseum

museumHalls :: CardDef
museumHalls =
  location
    "02127"
    "Museum Halls"
    [Miskatonic]
    Square
    [Circle, Diamond, Triangle]
    TheMiskatonicMuseum

securityOffice_128 :: CardDef
securityOffice_128 =
  location
    "02128"
    "Security Office"
    [Miskatonic]
    Diamond
    [Square]
    TheMiskatonicMuseum

securityOffice_129 :: CardDef
securityOffice_129 =
  location
    "02129"
    "Security Office"
    [Miskatonic]
    Diamond
    [Square]
    TheMiskatonicMuseum

administrationOffice_130 :: CardDef
administrationOffice_130 =
  location
    "02130"
    "Administration Office"
    [Miskatonic]
    Triangle
    [Square]
    TheMiskatonicMuseum

administrationOffice_131 :: CardDef
administrationOffice_131 =
  location
    "02131"
    "Administration Office"
    [Miskatonic]
    Triangle
    [Square]
    TheMiskatonicMuseum

exhibitHallAthabaskanExhibit :: CardDef
exhibitHallAthabaskanExhibit =
  locationWithUnrevealed
    "02132"
    "Exhibit Hall"
    [Miskatonic, Exhibit]
    NoSymbol
    [Square]
    ("Exhibit Hall" <:> "Athabaskan Exhibit")
    [Miskatonic, Exhibit]
    Plus
    [Square]
    TheMiskatonicMuseum

exhibitHallMedusaExhibit :: CardDef
exhibitHallMedusaExhibit =
  victory 1
    $ locationWithUnrevealed
      "02133"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Medusa Exhibit")
      [Miskatonic, Exhibit]
      T
      [Square, Moon]
      TheMiskatonicMuseum

exhibitHallNatureExhibit :: CardDef
exhibitHallNatureExhibit =
  victory 1
    $ locationWithUnrevealed
      "02134"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Nature Exhibit")
      [Miskatonic, Exhibit]
      Hourglass
      [Square, Squiggle]
      TheMiskatonicMuseum

exhibitHallEgyptianExhibit :: CardDef
exhibitHallEgyptianExhibit =
  victory 1
    $ locationWithUnrevealed
      "02135"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Egyptian Exhibit")
      [Miskatonic, Exhibit]
      Moon
      [Square, T]
      TheMiskatonicMuseum

exhibitHallHallOfTheDead :: CardDef
exhibitHallHallOfTheDead =
  victory 1
    $ locationWithUnrevealed
      "02136"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Hall of the Dead")
      [Miskatonic, Exhibit]
      Squiggle
      [Square, Hourglass]
      TheMiskatonicMuseum

exhibitHallRestrictedHall :: CardDef
exhibitHallRestrictedHall =
  victory 1
    $ locationWithUnrevealed
      "02137"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Restricted Hall")
      [Miskatonic, Exhibit]
      Equals
      [Square]
      TheMiskatonicMuseum

passengerCar_167 :: CardDef
passengerCar_167 =
  locationWithUnrevealed
    "02167"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_168 :: CardDef
passengerCar_168 =
  locationWithUnrevealed
    "02168"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_169 :: CardDef
passengerCar_169 =
  locationWithUnrevealed
    "02169"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_170 :: CardDef
passengerCar_170 =
  locationWithUnrevealed
    "02170"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

passengerCar_171 :: CardDef
passengerCar_171 =
  locationWithUnrevealed
    "02171"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Passenger Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

sleepingCar :: CardDef
sleepingCar =
  locationWithUnrevealed
    "02172"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Sleeping Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

diningCar :: CardDef
diningCar =
  locationWithUnrevealed
    "02173"
    "Train Car"
    [Train]
    NoSymbol
    []
    "Dining Car"
    [Train]
    NoSymbol
    []
    TheEssexCountyExpress

parlorCar :: CardDef
parlorCar =
  victory 1
    $ locationWithUnrevealed
      "02174"
      "Train Car"
      [Train]
      NoSymbol
      []
      "Parlor Car"
      [Train]
      NoSymbol
      []
      TheEssexCountyExpress

engineCar_175 :: CardDef
engineCar_175 =
  victory 1
    $ location "02175" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_176 :: CardDef
engineCar_176 =
  victory 1
    $ location "02176" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_177 :: CardDef
engineCar_177 =
  victory 1
    $ location "02177" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

villageCommons :: CardDef
villageCommons =
  location
    "02201"
    "Village Commons"
    [Dunwich, Central]
    Plus
    [Square, Circle, Moon]
    BloodOnTheAltar

bishopsBrook_202 :: CardDef
bishopsBrook_202 =
  location
    "02202"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    BloodOnTheAltar

bishopsBrook_203 :: CardDef
bishopsBrook_203 =
  location
    "02203"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    BloodOnTheAltar

burnedRuins_204 :: CardDef
burnedRuins_204 =
  location
    "02204"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    BloodOnTheAltar

burnedRuins_205 :: CardDef
burnedRuins_205 =
  location
    "02205"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    BloodOnTheAltar

osbornsGeneralStore_206 :: CardDef
osbornsGeneralStore_206 =
  location
    "02206"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    BloodOnTheAltar

osbornsGeneralStore_207 :: CardDef
osbornsGeneralStore_207 =
  location
    "02207"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    BloodOnTheAltar

congregationalChurch_208 :: CardDef
congregationalChurch_208 =
  location
    "02208"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    BloodOnTheAltar

congregationalChurch_209 :: CardDef
congregationalChurch_209 =
  location
    "02209"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    BloodOnTheAltar

houseInTheReeds_210 :: CardDef
houseInTheReeds_210 =
  location
    "02210"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    BloodOnTheAltar

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 =
  location
    "02211"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    BloodOnTheAltar

schoolhouse_212 :: CardDef
schoolhouse_212 =
  location
    "02212"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    BloodOnTheAltar

schoolhouse_213 :: CardDef
schoolhouse_213 =
  location
    "02213"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    BloodOnTheAltar

theHiddenChamber :: CardDef
theHiddenChamber =
  victory 2
    $ singleSided
    $ location
      "02214"
      ("The Hidden Chamber" <:> "Prison of the Beast")
      [Dunwich]
      NoSymbol
      []
      BloodOnTheAltar

dunwichVillage_242 :: CardDef
dunwichVillage_242 =
  location
    "02242"
    "Dunwich Village"
    [Dunwich]
    Circle
    [Triangle, Square, Diamond]
    UndimensionedAndUnseen

dunwichVillage_243 :: CardDef
dunwichVillage_243 =
  location
    "02243"
    "Dunwich Village"
    [Dunwich]
    Circle
    [Triangle, Square, Diamond]
    UndimensionedAndUnseen

coldSpringGlen_244 :: CardDef
coldSpringGlen_244 =
  location
    "02244"
    "Cold Spring Glen"
    [Dunwich]
    Triangle
    [Circle, Diamond, Plus]
    UndimensionedAndUnseen

coldSpringGlen_245 :: CardDef
coldSpringGlen_245 =
  location
    "02245"
    "Cold Spring Glen"
    [Dunwich]
    Triangle
    [Circle, Diamond, Plus]
    UndimensionedAndUnseen

tenAcreMeadow_246 :: CardDef
tenAcreMeadow_246 =
  location
    "02246"
    "Ten-Acre Meadow"
    [Dunwich]
    Diamond
    [Circle, Triangle, Plus]
    UndimensionedAndUnseen

tenAcreMeadow_247 :: CardDef
tenAcreMeadow_247 =
  location
    "02247"
    "Ten-Acre Meadow"
    [Dunwich]
    Diamond
    [Circle, Triangle, Plus]
    UndimensionedAndUnseen

blastedHeath_248 :: CardDef
blastedHeath_248 =
  location
    "02248"
    "Blasted Heath"
    [Dunwich]
    Square
    [Circle, Hourglass]
    UndimensionedAndUnseen

blastedHeath_249 :: CardDef
blastedHeath_249 =
  location
    "02249"
    "Blasted Heath"
    [Dunwich]
    Square
    [Circle, Hourglass]
    UndimensionedAndUnseen

whateleyRuins_250 :: CardDef
whateleyRuins_250 =
  location
    "02250"
    "Whateley Ruins"
    [Dunwich]
    Plus
    [Triangle, Diamond, Hourglass]
    UndimensionedAndUnseen

whateleyRuins_251 :: CardDef
whateleyRuins_251 =
  location
    "02251"
    "Whateley Ruins"
    [Dunwich]
    Plus
    [Triangle, Diamond, Hourglass]
    UndimensionedAndUnseen

devilsHopYard_252 :: CardDef
devilsHopYard_252 =
  location
    "02252"
    "Devil's Hop Yard"
    [Dunwich]
    Hourglass
    [Square, Plus]
    UndimensionedAndUnseen

devilsHopYard_253 :: CardDef
devilsHopYard_253 =
  location
    "02253"
    "Devil's Hop Yard"
    [Dunwich]
    Hourglass
    [Square, Plus]
    UndimensionedAndUnseen

baseOfTheHill :: CardDef
baseOfTheHill =
  location
    "02282"
    "Base of the Hill"
    [Dunwich, SentinelHill]
    Triangle
    [Square, Plus, Squiggle, Hourglass]
    WhereDoomAwaits

ascendingPath :: CardDef
ascendingPath =
  location
    "02283"
    "Ascending Path"
    [Dunwich, SentinelHill]
    Square
    [Triangle, Diamond, T, Equals, Moon]
    WhereDoomAwaits

sentinelPeak :: CardDef
sentinelPeak =
  victory 2
    $ location
      "02284"
      "Sentinel Peak"
      [Dunwich, SentinelHill]
      Diamond
      [Square]
      WhereDoomAwaits

slaughteredWoods :: CardDef
slaughteredWoods =
  locationWithUnrevealed
    "02285"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Slaughtered Woods"
    [Dunwich, Woods]
    Plus
    [Triangle, Hourglass]
    WhereDoomAwaits

eerieGlade :: CardDef
eerieGlade =
  locationWithUnrevealed
    "02286"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Eerie Glade"
    [Dunwich, Woods]
    Hourglass
    [Triangle, Plus]
    WhereDoomAwaits

destroyedPath :: CardDef
destroyedPath =
  locationWithUnrevealed
    "02287"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Destroyed Path"
    [Dunwich, Woods]
    Squiggle
    [Triangle, Equals]
    WhereDoomAwaits

frozenSpring :: CardDef
frozenSpring =
  locationWithUnrevealed
    "02288"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Frozen Spring"
    [Dunwich, Woods]
    Plus
    [Triangle, Hourglass]
    WhereDoomAwaits

dimensionalGap :: CardDef
dimensionalGap =
  locationWithUnrevealed
    "02289"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Dimensional Gap"
    [Dunwich, Woods, Altered]
    T
    [Square, Moon]
    WhereDoomAwaits

aTearInThePath :: CardDef
aTearInThePath =
  locationWithUnrevealed
    "02290"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "A Tear in the Path"
    [Dunwich, Woods, Altered]
    Equals
    [Square, Squiggle]
    WhereDoomAwaits

uprootedWoods :: CardDef
uprootedWoods =
  locationWithUnrevealed
    "02291"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Uprooted Woods"
    [Dunwich, Woods, Altered]
    Moon
    [Square, T]
    WhereDoomAwaits

lostMemories :: CardDef
lostMemories =
  locationWithUnrevealed
    "02292"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Lost Memories"
    [Dunwich, Woods, Altered]
    T
    [Square, Moon]
    WhereDoomAwaits

anotherDimension :: CardDef
anotherDimension =
  location
    "02320"
    ("Another Dimension" <:> "Unfettered by Reality")
    [Otherworld]
    Circle
    [Square, Diamond, Triangle]
    LostInTimeAndSpace

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  location
    "02321"
    "The Edge of the Universe"
    [Otherworld]
    Moon
    [Plus, Squiggle]
    LostInTimeAndSpace

tearThroughTime :: CardDef
tearThroughTime =
  location
    "02322"
    "Tear Through Time"
    [Otherworld]
    Moon
    [Circle, Plus, Squiggle]
    LostInTimeAndSpace

tearThroughSpace :: CardDef
tearThroughSpace =
  singleSided
    $ ( location
          "02324"
          "Tear Through Space"
          [Otherworld, Extradimensional]
          Square
          [Diamond, Triangle, Square]
          LostInTimeAndSpace
      )
      { cdKeywords = setFromList [Keyword.Surge]
      , cdEncounterSetQuantity = Just 4
      }

prismaticCascade :: CardDef
prismaticCascade =
  singleSided
    $ ( location
          "02325"
          "Prismatic Cascade"
          [Otherworld, Extradimensional]
          Diamond
          [Square, Plus]
          LostInTimeAndSpace
      )
      { cdEncounterSetQuantity = Just 2
      }

endlessBridge :: CardDef
endlessBridge =
  singleSided
    $ ( location
          "02326"
          "Endless Bridge"
          [Otherworld, Extradimensional]
          Triangle
          [Square, Squiggle]
          LostInTimeAndSpace
      )
      { cdEncounterSetQuantity = Just 2
      }

stepsOfYhagharl :: CardDef
stepsOfYhagharl =
  singleSided
    $ location
      "02327"
      "Steps of Y'hagharl"
      [Otherworld, Extradimensional]
      Plus
      [Diamond, Moon]
      LostInTimeAndSpace

dimensionalDoorway :: CardDef
dimensionalDoorway =
  singleSided
    $ location
      "02328"
      "Dimensional Doorway"
      [Otherworld, Extradimensional]
      Squiggle
      [Triangle, Moon]
      LostInTimeAndSpace
