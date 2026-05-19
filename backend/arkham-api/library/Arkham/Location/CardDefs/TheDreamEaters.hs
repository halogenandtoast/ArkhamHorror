{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDreamEaters where

import Arkham.Location.CardDefs.Import

dreamGateWondrousJourney :: CardDef
dreamGateWondrousJourney =
  (emptyCardDef "06015a" ("Dream-Gate" <:> "Wondrous Journey") LocationType)
    { cdRevealedName = Just $ "Dream-Gate" <:> "Wondrous Journey"
    , cdCardTraits = setFromList [Dreamlands]
    , cdRevealedCardTraits = setFromList [Dreamlands]
    , cdArt = "06015a"
    , cdLocationSymbol = Just NoSymbol
    , cdLocationRevealedSymbol = Just NoSymbol
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdClassSymbols = singleton #neutral
    , cdLevel = Nothing
    , cdOtherSide = Just "06015b"
    }

dreamGatePointlessReality :: CardDef
dreamGatePointlessReality =
  (emptyCardDef "06015b" ("Dream-Gate" <:> "Pointless Reality") LocationType)
    { cdRevealedName = Just $ "Dream-Gate" <:> "Pointless Reality"
    , cdCardTraits = setFromList [Dreamlands]
    , cdRevealedCardTraits = setFromList [Dreamlands]
    , cdDoubleSided = False
    , cdArt = "06015b"
    , cdLocationSymbol = Just NoSymbol
    , cdLocationRevealedSymbol = Just NoSymbol
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdClassSymbols = singleton #neutral
    , cdLevel = Nothing
    }

seventySteps :: CardDef
seventySteps =
  location
    "06045"
    ("Seventy Steps" <:> "Of Lighter Slumber")
    [Steps]
    Heart
    [Hourglass]
    BeyondTheGatesOfSleep

theCavernOfFlame :: CardDef
theCavernOfFlame =
  location
    "06046"
    "The Cavern of Flame"
    [Cave, Steps]
    Hourglass
    [Heart, Equals]
    BeyondTheGatesOfSleep

sevenHundredSteps :: CardDef
sevenHundredSteps =
  location
    "06047"
    ("Seven Hundred Steps" <:> "Of Deeper Slumber")
    [Steps]
    Equals
    [Hourglass, T]
    BeyondTheGatesOfSleep

baseOfTheSteps :: CardDef
baseOfTheSteps =
  location
    "06048"
    "Base of the Steps"
    [Steps, Woods]
    T
    [Equals, Squiggle]
    BeyondTheGatesOfSleep

theEnchantedPath :: CardDef
theEnchantedPath =
  location
    "06049"
    "The Enchanted Path"
    [Woods]
    Squiggle
    [T, Moon]
    BeyondTheGatesOfSleep

enchantedWoodsMysticalForest :: CardDef
enchantedWoodsMysticalForest =
  victory 2
    $ locationWithUnrevealed
      "06050"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Mystical Forest")
      [Woods]
      Circle
      [Squiggle, Star, Plus]
      BeyondTheGatesOfSleep

enchantedWoodsVillageOfZoogs :: CardDef
enchantedWoodsVillageOfZoogs =
  victory 2
    $ locationWithUnrevealed
      "06051"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Village of Zoogs")
      [Woods]
      Diamond
      [Squiggle, Square, Star]
      BeyondTheGatesOfSleep

enchantedWoodsGreatStoneCircle :: CardDef
enchantedWoodsGreatStoneCircle =
  victory 1
    $ locationWithUnrevealed
      "06052"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Great Stone Circle")
      [Woods]
      Triangle
      [Squiggle, Plus, Square]
      BeyondTheGatesOfSleep

enchantedWoodsStoneTrapdoor :: CardDef
enchantedWoodsStoneTrapdoor =
  victory 2
    $ locationWithUnrevealed
      "06053"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Stone Trapdoor")
      [Woods]
      Square
      [Squiggle, Triangle, Diamond]
      BeyondTheGatesOfSleep

enchantedWoodsTheMoonTree :: CardDef
enchantedWoodsTheMoonTree =
  victory 2
    $ locationWithUnrevealed
      "06054"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "The Moon-Tree")
      [Woods]
      Star
      [Squiggle, Diamond, Circle]
      BeyondTheGatesOfSleep

enchantedWoodsFungalForest :: CardDef
enchantedWoodsFungalForest =
  victory 2
    $ locationWithUnrevealed
      "06055"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Fungal Forest")
      [Woods]
      Plus
      [Squiggle, Circle, Triangle]
      BeyondTheGatesOfSleep

enchantedWoodsLostWoods :: CardDef
enchantedWoodsLostWoods =
  victory 2
    $ locationWithUnrevealed
      "06056"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Lost Woods")
      [Woods]
      Droplet
      [Squiggle]
      BeyondTheGatesOfSleep

waitingRoom :: CardDef
waitingRoom =
  location
    "06070"
    "Waiting Room"
    [StMarys]
    Circle
    [Diamond, Triangle, Square]
    WakingNightmare

emergencyRoom :: CardDef
emergencyRoom =
  victory 1
    $ location
      "06071"
      "Emergency Room"
      [StMarys]
      Square
      [Circle, Triangle]
      WakingNightmare

experimentalTherapiesWard :: CardDef
experimentalTherapiesWard =
  victory 2
    $ location
      "06072"
      "Experimental Therapies Ward"
      [StMarys]
      Triangle
      [Circle, Square, Heart]
      WakingNightmare

recordsOffice :: CardDef
recordsOffice =
  victory 2
    $ location
      "06073"
      "Records Office"
      [StMarys]
      Diamond
      [Circle]
      WakingNightmare

stairwell :: CardDef
stairwell =
  victory 1
    $ location
      "06074"
      "Stairwell"
      [StMarys]
      Heart
      [Triangle, Plus, Hourglass, T, Moon]
      WakingNightmare

morgue :: CardDef
morgue =
  victory 2
    $ locationWithUnrevealed
      "06075"
      "Basement Door"
      [StMarys, Basement]
      Plus
      [Heart]
      "Morgue"
      [StMarys, Basement]
      Hourglass
      [Heart]
      WakingNightmare

operatingRoom :: CardDef
operatingRoom =
  victory 2
    $ locationWithUnrevealed
      "06076"
      "Basement Door"
      [StMarys, Basement]
      Plus
      [Heart]
      "Operating Room"
      [StMarys, Basement]
      T
      [Heart]
      WakingNightmare

privateRoom :: CardDef
privateRoom =
  locationWithUnrevealed
    "06077"
    "Basement Door"
    [StMarys, Basement]
    Plus
    [Heart]
    "Private Room"
    [StMarys, Basement]
    Moon
    [Heart]
    WakingNightmare

ulthar :: CardDef
ulthar = veiled $ location "06127" "Ulthar" [Skai, City] Heart [Squiggle] TheSearchForKadath

skaiRiver :: CardDef
skaiRiver =
  singleSided
    $ location "06128" "Skai River" [Skai, City] Squiggle [Heart, Triangle] TheSearchForKadath

dylathLeen :: CardDef
dylathLeen = veiled $ location "06129" "Dylath-Leen" [Skai, City, Port] Triangle [Squiggle] TheSearchForKadath

kadatheron :: CardDef
kadatheron =
  veiled
    $ location
      "06130"
      "Kadatheron"
      [Mnar, Ancient, City, Port]
      Circle
      [Square, Diamond]
      TheSearchForKadath

sarnath :: CardDef
sarnath =
  victory 1
    $ veiled
    $ location "06131" "Sarnath" [Mnar, Ancient, Ruins] Diamond [Circle, Square] TheSearchForKadath

ruinsOfIb :: CardDef
ruinsOfIb =
  victory 1
    $ veiled
    $ location "06132" "Ruins of Ib" [Mnar, Ancient, Ruins] Square [Circle, Diamond] TheSearchForKadath

ilekVad :: CardDef
ilekVad =
  victory 1
    $ veiled
    $ location "06133" "Ilek-Vad" [Forbidden, City, Port] Circle [Diamond] TheSearchForKadath

forbiddenLands :: CardDef
forbiddenLands =
  singleSided
    $ location "06134" "Forbidden Lands" [Forbidden, Wastes] Diamond [Circle, Square] TheSearchForKadath

zulanThek :: CardDef
zulanThek =
  veiled
    $ location "06135" "Zulan-Thek" [Forbidden, City] Square [Diamond] TheSearchForKadath

baharna :: CardDef
baharna =
  victory 1
    $ veiled
    $ location "06136" "Baharna" [Oriab, City, Port] Circle [Square, Diamond] TheSearchForKadath

mtNgranek :: CardDef
mtNgranek =
  victory 1
    $ veiled
    $ location "06137" "Mt. Ngranek" [Oriab, Mountain] Square [Circle, Diamond] TheSearchForKadath

namelessRuins :: CardDef
namelessRuins =
  victory 1
    $ veiled
    $ location
      "06138"
      "Nameless Ruins"
      [Oriab, Ancient, Ruins]
      Diamond
      [Circle, Square]
      TheSearchForKadath

celephais :: CardDef
celephais =
  victory 1
    $ veiled
    $ location
      "06139"
      "Celephaïs"
      [OothNargai, City, Port]
      Hourglass
      [Moon, Plus]
      TheSearchForKadath

serannian :: CardDef
serannian =
  victory 1
    $ veiled
    $ location
      "06140"
      "Serannian"
      [OothNargai, City, Port]
      Moon
      [Hourglass]
      TheSearchForKadath

hazuthKleg :: CardDef
hazuthKleg =
  veiled
    $ location
      "06141"
      "Hazuth-Kleg"
      [OothNargai, City]
      Plus
      [Hourglass, T]
      TheSearchForKadath

templeOfUnattainableDesires :: CardDef
templeOfUnattainableDesires =
  victory 1
    $ veiled
    $ location
      "06142"
      "Temple of Unattainable Desires"
      [OothNargai, Temple]
      T
      [Plus, Star]
      TheSearchForKadath

cityWhichAppearsOnNoMap :: CardDef
cityWhichAppearsOnNoMap =
  victory 2
    $ veiled
    $ location
      "06143"
      "City-Which-Appears-On-No-Map"
      [City, Otherworld]
      Star
      [T]
      TheSearchForKadath

burialGround :: CardDef
burialGround =
  victory 1
    $ location
      "06174"
      "Burial Ground"
      [Graveyard]
      Moon
      [Square, Plus]
      AThousandShapesOfHorror

frontPorchEntryway :: CardDef
frontPorchEntryway =
  locationWithUnrevealed
    "06175"
    "Front Porch"
    mempty
    Square
    [Moon, Heart, Hourglass]
    "Entryway"
    mempty
    Square
    [Moon, Heart, Hourglass]
    AThousandShapesOfHorror

downstairsDoorwayDen :: CardDef
downstairsDoorwayDen =
  victory 1
    $ locationWithUnrevealed
      "06176"
      "Downstairs Doorway"
      mempty
      Hourglass
      [Square]
      "Den"
      mempty
      Hourglass
      [Square]
      AThousandShapesOfHorror

downstairsDoorwayParlor :: CardDef
downstairsDoorwayParlor =
  victory 1
    $ locationWithUnrevealed
      "06177"
      "Downstairs Doorway"
      mempty
      Hourglass
      [Square]
      "Parlor"
      mempty
      Hourglass
      [Square]
      AThousandShapesOfHorror

upstairsHallway :: CardDef
upstairsHallway =
  location
    "06178"
    "Upstairs Hallway"
    mempty
    Heart
    [Diamond, Square, Circle]
    AThousandShapesOfHorror

upstairsDoorwayLibrary :: CardDef
upstairsDoorwayLibrary =
  victory 1
    $ locationWithUnrevealed
      "06179"
      "Upstairs Doorway"
      mempty
      Diamond
      [Heart]
      "Library"
      mempty
      Diamond
      [Heart]
      AThousandShapesOfHorror

upstairsDoorwayBedroom :: CardDef
upstairsDoorwayBedroom =
  victory 1
    $ locationWithUnrevealed
      "06180"
      "Upstairs Doorway"
      mempty
      Diamond
      [Heart]
      "Bedroom"
      mempty
      Diamond
      [Heart]
      AThousandShapesOfHorror

attic_AThousandShapesOfHorror :: CardDef
attic_AThousandShapesOfHorror =
  victory 1
    $ location
      "06181"
      "Attic"
      mempty
      Circle
      [Heart]
      AThousandShapesOfHorror

unmarkedTomb :: CardDef
unmarkedTomb =
  victory 1
    $ location
      "06182"
      "Unmarked Tomb"
      [Graveyard]
      Plus
      [Moon]
      AThousandShapesOfHorror

mysteriousStairs_183 :: CardDef
mysteriousStairs_183 =
  location
    "06183"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_184 :: CardDef
mysteriousStairs_184 =
  location
    "06184"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_185 :: CardDef
mysteriousStairs_185 =
  location
    "06185"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_186 :: CardDef
mysteriousStairs_186 =
  location
    "06186"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_187 :: CardDef
mysteriousStairs_187 =
  location
    "06187"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_188 :: CardDef
mysteriousStairs_188 =
  location
    "06188"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

moonBeastGalley :: CardDef
moonBeastGalley =
  storyOnBack
    $ location
      "06214"
      "Moon-Beast Galley"
      [Ship]
      NoSymbol
      []
      DarkSideOfTheMoon

cityOfTheMoonBeasts :: CardDef
cityOfTheMoonBeasts =
  victory 1
    $ location
      "06215"
      "City of the Moon-Beasts"
      [Surface, City]
      Triangle
      [Square, Moon]
      DarkSideOfTheMoon

theDarkCrater :: CardDef
theDarkCrater =
  victory 1
    $ location
      "06216"
      "The Dark Crater"
      [Surface]
      Moon
      [Triangle, Circle, Squiggle]
      DarkSideOfTheMoon

templeOfTheMoonLizard :: CardDef
templeOfTheMoonLizard =
  victory 1
    $ location
      "06217"
      "Temple of the Moon Lizard"
      [Surface]
      Square
      [Circle, Triangle]
      DarkSideOfTheMoon

moonForest :: CardDef
moonForest =
  victory 1
    $ location
      "06218"
      "Moon-Forest"
      [Surface, Woods]
      Circle
      [Moon, Square, Squiggle]
      DarkSideOfTheMoon

cavernsBeneathTheMoonDarkSide :: CardDef
cavernsBeneathTheMoonDarkSide =
  location
    "06219"
    ("Caverns Beneath the Moon" <:> "Dark Side")
    [Cave]
    Squiggle
    [Circle, Moon, Star]
    DarkSideOfTheMoon

theBlackCore :: CardDef
theBlackCore =
  location
    "06220"
    "The Black Core"
    [Cave]
    Star
    [Squiggle, Equals]
    DarkSideOfTheMoon

cavernsBeneathTheMoonLightSide :: CardDef
cavernsBeneathTheMoonLightSide =
  location
    "06221"
    ("Caverns Beneath the Moon" <:> "Light Side")
    [Cave]
    Equals
    [Star, Hourglass]
    DarkSideOfTheMoon

lightSideOfTheMoon :: CardDef
lightSideOfTheMoon =
  victory 2
    $ location
      "06222"
      "Light Side of the Moon"
      [Surface, Ruins]
      Hourglass
      [Equals, Heart]
      DarkSideOfTheMoon

theWhiteShip :: CardDef
theWhiteShip =
  location
    "06223"
    "The White Ship"
    [Ship]
    Heart
    [Hourglass]
    DarkSideOfTheMoon

vaultsOfZin :: CardDef
vaultsOfZin =
  victory 1
    $ veiled
    $ location
      "06254"
      "Vaults of Zin"
      []
      Heart
      [T, Moon]
      PointOfNoReturn

cityOfGugs :: CardDef
cityOfGugs =
  victory 1
    $ otherSideIs "06255b"
    $ location
      "06255"
      "City of Gugs"
      []
      T
      [Heart, Squiggle, Moon]
      PointOfNoReturn

towerOfKoth :: CardDef
towerOfKoth =
  otherSideIs "06256b"
    $ location
      "06256"
      "Tower of Koth"
      []
      Squiggle
      [T, Square]
      PointOfNoReturn

plainOfTheGhouls :: CardDef
plainOfTheGhouls =
  victory 1
    $ veiled
    $ location
      "06257"
      "Plain of the Ghouls"
      [Central]
      Moon
      [Heart, T, Hourglass]
      PointOfNoReturn

cragOfTheGhouls :: CardDef
cragOfTheGhouls =
  victory 1
    $ veiled
    $ location
      "06258"
      "Crag of the Ghouls"
      [Vale]
      Hourglass
      [Equals, Circle, Moon]
      PointOfNoReturn

seaOfBones :: CardDef
seaOfBones =
  victory 1
    $ otherSideIs "06259b"
    $ location
      "06259"
      "Sea of Bones"
      [Vale]
      Circle
      [Hourglass, Star, Equals]
      PointOfNoReturn

peaksOfThok :: CardDef
peaksOfThok =
  otherSideIs "06260b"
    $ location
      "06260"
      "Peaks of Thok"
      [Vale, Central]
      Star
      [Equals, Circle]
      PointOfNoReturn

valeOfPnath :: CardDef
valeOfPnath =
  victory 1
    $ veiled
    $ location
      "06261"
      "Vale of Pnath"
      [Vale]
      Equals
      [Hourglass, Star, Circle, Plus]
      PointOfNoReturn

seaOfPitch_262 :: CardDef
seaOfPitch_262 =
  veiled
    $ location
      "06262"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_263 :: CardDef
seaOfPitch_263 =
  veiled
    $ location
      "06263"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_264 :: CardDef
seaOfPitch_264 =
  veiled
    $ location
      "06264"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_265 :: CardDef
seaOfPitch_265 =
  veiled
    $ location
      "06265"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

plateauOfLengWhereTheGodsDwell :: CardDef
plateauOfLengWhereTheGodsDwell =
  location
    "06295"
    "Plateau of Leng"
    [Leng]
    Diamond
    [Triangle]
    WhereTheGodsDwell

coldWastes :: CardDef
coldWastes =
  location
    "06296"
    "Cold Wastes"
    [Leng]
    Triangle
    [Diamond, Plus, T]
    WhereTheGodsDwell

monasteryOfLeng :: CardDef
monasteryOfLeng =
  location
    "06297"
    "Monastery of Leng"
    [Leng]
    Plus
    [Triangle]
    WhereTheGodsDwell

onyxGates :: CardDef
onyxGates =
  location
    "06298"
    "Onyx Gates"
    [Leng, Kadath]
    T
    [Triangle, Square]
    WhereTheGodsDwell

theOnyxCastle :: CardDef
theOnyxCastle =
  locationWithUnrevealed
    "06299"
    "The Onyx Castle"
    [Kadath]
    Square
    [T]
    "The Great Hall"
    [Kadath]
    Square
    [Equals]
    WhereTheGodsDwell

forsakenTowerOfIllusionAndMyth :: CardDef
forsakenTowerOfIllusionAndMyth =
  locationWithUnrevealed
    "06300"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Illusion and Myth")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfLifeAndDeath :: CardDef
forsakenTowerOfLifeAndDeath =
  locationWithUnrevealed
    "06301"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Life and Death")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfInfiniteTruth :: CardDef
forsakenTowerOfInfiniteTruth =
  locationWithUnrevealed
    "06302"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Infinite Truth")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfEternalFlame :: CardDef
forsakenTowerOfEternalFlame =
  locationWithUnrevealed
    "06303"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Eternal Flame")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfTheQueenOfNight :: CardDef
forsakenTowerOfTheQueenOfNight =
  locationWithUnrevealed
    "06304"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of the Queen of Night")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfPrimevalLight :: CardDef
forsakenTowerOfPrimevalLight =
  locationWithUnrevealed
    "06305"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Primeval Light")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

theGreatWebWebStairs :: CardDef
theGreatWebWebStairs =
  quantity 3
    $ locationWithUnrevealed
      "06340"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Web-Stairs")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebCosmicWeb :: CardDef
theGreatWebCosmicWeb =
  quantity 2
    $ locationWithUnrevealed
      "06341"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Cosmic Web")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebTangledWeb :: CardDef
theGreatWebTangledWeb =
  quantity 3
    $ locationWithUnrevealed
      "06342"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Tangled Web")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebPrisonOfCocoons :: CardDef
theGreatWebPrisonOfCocoons =
  quantity 2
    $ locationWithUnrevealed
      "06343"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Prison of Cocoons")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebVastWeb :: CardDef
theGreatWebVastWeb =
  quantity 2
    $ locationWithUnrevealed
      "06344"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Vast Web")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebWebWovenIsland :: CardDef
theGreatWebWebWovenIsland =
  quantity 3
    $ locationWithUnrevealed
      "06345"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Web-Woven Island")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos
