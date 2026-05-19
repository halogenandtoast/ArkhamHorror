module Arkham.Location.CardDefs.TheScarletKeys where

import Arkham.Location.CardDefs.Import
import Arkham.Keyword qualified as Keyword

rainyLondonStreets :: CardDef
rainyLondonStreets =
  location
    "09510"
    "Rainy London Streets"
    [London]
    Equals
    [Circle, Square, Triangle, Squiggle]
    RiddlesAndRain

bigBen :: CardDef
bigBen =
  victory 1
    $ location
      "09511"
      "Big Ben"
      [London]
      Triangle
      [Equals, Circle]
      RiddlesAndRain

westminsterAbbey :: CardDef
westminsterAbbey =
  location
    "09512"
    "Westminster Abbey"
    [London]
    Circle
    [Equals, Triangle]
    RiddlesAndRain

kensingtonGardens :: CardDef
kensingtonGardens =
  victory 1
    $ location
      "09513"
      "Kensington Gardens"
      [London]
      Square
      [Equals]
      RiddlesAndRain

theTowerBridge :: CardDef
theTowerBridge =
  location
    "09514"
    "The Tower Bridge"
    [London]
    Squiggle
    [Equals, Moon, T]
    RiddlesAndRain

traitorsGate :: CardDef
traitorsGate =
  location
    "09515"
    "Traitor's Gate"
    [London]
    T
    [Squiggle, Moon]
    RiddlesAndRain

towerOfLondon :: CardDef
towerOfLondon =
  location
    "09516"
    "Tower of London"
    [London]
    Moon
    [Squiggle, T, Hourglass]
    RiddlesAndRain

towerPrison :: CardDef
towerPrison =
  victory 1
    $ location
      "09517"
      "Tower Prison"
      [London]
      Hourglass
      [Moon]
      RiddlesAndRain

marrakeshRailwayStation :: CardDef
marrakeshRailwayStation =
  location
    "09526"
    "Marrakesh Railway Station"
    [Marrakesh]
    Squiggle
    [Diamond, Heart, Square]
    DeadHeat

jemaaElFnaaSquare :: CardDef
jemaaElFnaaSquare =
  location
    "09527"
    "Jemaa el-Fnaa Square"
    [Marrakesh]
    Diamond
    [Squiggle, Heart, Triangle]
    DeadHeat

saadiansTombs :: CardDef
saadiansTombs =
  location
    "09528"
    "Saadian's Tombs"
    [Marrakesh]
    Heart
    [Squiggle, Diamond]
    DeadHeat

tanneries :: CardDef
tanneries =
  location
    "09529"
    "Tanneries"
    [Marrakesh]
    Triangle
    [Diamond, Square]
    DeadHeat

bahiaPalaceGardens :: CardDef
bahiaPalaceGardens =
  locationWithUnrevealed
    "09530"
    "Bahia Palace Gardens"
    [Marrakesh]
    Square
    [Squiggle, Triangle]
    "Bahia Palace Gardens"
    [Marrakesh, RitualSite]
    Square
    [Squiggle, Triangle]
    DeadHeat

marrakeshRailwayStationAbandoned :: CardDef
marrakeshRailwayStationAbandoned =
  location
    "09531"
    "Marrakesh Railway Station"
    [Marrakesh, Abandoned]
    Squiggle
    [Diamond, Heart, Square]
    DeadHeat

jemaaElFnaaSquareAbandoned :: CardDef
jemaaElFnaaSquareAbandoned =
  location
    "09532"
    "Jemaa el-Fnaa Square"
    [Marrakesh, Abandoned]
    Diamond
    [Squiggle, Heart, Triangle]
    DeadHeat

saadiansTombsAbandoned :: CardDef
saadiansTombsAbandoned =
  location
    "09533"
    "Saadian's Tombs"
    [Marrakesh, Abandoned]
    Heart
    [Squiggle, Diamond]
    DeadHeat

tanneriesAbandoned :: CardDef
tanneriesAbandoned =
  location
    "09534"
    "Tanneries"
    [Marrakesh, Abandoned]
    Triangle
    [Diamond, Square]
    DeadHeat

bahiaPalaceGardensAbandoned :: CardDef
bahiaPalaceGardensAbandoned =
  locationWithUnrevealed
    "09535"
    "Bahia Palace Gardens"
    [Marrakesh, Abandoned]
    Square
    [Squiggle, Triangle]
    "Bahia Palace Gardens"
    [Marrakesh, RitualSite, Abandoned]
    Square
    [Squiggle, Triangle]
    DeadHeat

avenidaDeMayo :: CardDef
avenidaDeMayo =
  location
    "09549"
    "Avenida de Mayo"
    [BuenosAires, Central]
    Equals
    [Moon, T, Square, Circle]
    SanguineShadows

casaRosada :: CardDef
casaRosada =
  location
    "09550"
    "Casa Rosada"
    [BuenosAires]
    Moon
    [Equals, Moon]
    SanguineShadows

catedralMetropolitana :: CardDef
catedralMetropolitana =
  location
    "09551"
    "Catedral Metropolitana"
    [BuenosAires]
    Moon
    [Equals, Moon]
    SanguineShadows

cementarioDeLaRecoleta :: CardDef
cementarioDeLaRecoleta =
  location
    "09552"
    "Cementario de la Recoleta"
    [BuenosAires]
    T
    [Equals, T]
    SanguineShadows

palacioErrazuriz :: CardDef
palacioErrazuriz =
  location
    "09553"
    "Palacio Errázuriz"
    [BuenosAires]
    T
    [Equals, T]
    SanguineShadows

theCabildo :: CardDef
theCabildo =
  location
    "09554"
    "The Cabildo"
    [BuenosAires]
    Square
    [Equals, Square]
    SanguineShadows

bancoDeLaProvincia :: CardDef
bancoDeLaProvincia =
  location
    "09555"
    "Banco de la Provincia"
    [BuenosAires]
    Square
    [Equals, Square]
    SanguineShadows

teatroColon :: CardDef
teatroColon =
  location
    "09556"
    "Teatro Colón"
    [BuenosAires]
    Circle
    [Equals]
    SanguineShadows

galataDocks :: CardDef
galataDocks =
  victory 1
    $ location
      "09572"
      "Galata Docks"
      [Istanbul]
      Squiggle
      [Diamond, Plus]
      DealingsInTheDark

galata :: CardDef
galata =
  location
    "09573"
    "Galata"
    [Istanbul]
    Plus
    [Moon, Circle, Squiggle]
    DealingsInTheDark

obeliskOfTheodosius :: CardDef
obeliskOfTheodosius =
  victory 1
    $ location
      "09574"
      "Obelisk of Theodosius"
      [Istanbul]
      Moon
      [Plus, Square]
      DealingsInTheDark

istanbulUniversity :: CardDef
istanbulUniversity =
  victory 1
    $ location
      "09575"
      "Istanbul University"
      [Istanbul]
      Circle
      [Plus, Square]
      DealingsInTheDark

hagiaSophia :: CardDef
hagiaSophia =
  location
    "09576"
    "Hagia Sophia"
    [Istanbul]
    Square
    [Moon, Circle]
    DealingsInTheDark

grandBazaarDarkenedAlley :: CardDef
grandBazaarDarkenedAlley =
  locationWithUnrevealed_
    "09577"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Darkened Alley")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarPublicBaths :: CardDef
grandBazaarPublicBaths =
  locationWithUnrevealed_
    "09578"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Public Baths")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarMarbleFountain :: CardDef
grandBazaarMarbleFountain =
  victory 1
    $ locationWithUnrevealed_
      "09579"
      "Grand Bazaar"
      [Istanbul, Bazaar]
      ("Grand Bazaar" <:> "Marble Fountain")
      [Istanbul, Bazaar]
      DealingsInTheDark

grandBazaarCrowdedShops :: CardDef
grandBazaarCrowdedShops =
  locationWithUnrevealed_
    "09580"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Crowded Shops")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarBusyWalkway :: CardDef
grandBazaarBusyWalkway =
  locationWithUnrevealed_
    "09581"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Busy Walkway")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarRooftopAccess :: CardDef
grandBazaarRooftopAccess =
  victory 1
    $ locationWithUnrevealed_
      "09582"
      "Grand Bazaar"
      [Istanbul, Bazaar]
      ("Grand Bazaar" <:> "Rooftop Access")
      [Istanbul, Bazaar]
      DealingsInTheDark

grandBazaarJewelersRoad :: CardDef
grandBazaarJewelersRoad =
  locationWithUnrevealed_
    "09583"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Jewelers' Road")
    [Istanbul, Bazaar]
    DealingsInTheDark

cafeLunaCoterieHaunt :: CardDef
cafeLunaCoterieHaunt =
  otherSideIs "09600b"
    $ location
      "09600a"
      ("Café Luna" <:> "Coterie Haunt")
      [Havana]
      Spade
      [Squiggle, T]
      DancingMad

cafeLunaBastionOfRemembrance :: CardDef
cafeLunaBastionOfRemembrance =
  otherSideIs "09600a"
    $ location
      "09600b"
      ("Café Luna" <:> "Bastion of Remembrance")
      [Havana]
      Spade
      [Squiggle, T]
      DancingMad

elMalecon :: CardDef
elMalecon =
  victory 1
    $ location
      "09601"
      "El Malecón"
      [Havana]
      T
      [Spade, Squiggle, Equals]
      DancingMad

jardinesDeLaTropical :: CardDef
jardinesDeLaTropical =
  victory 1
    $ location
      "09602"
      "Jardines de la Tropical"
      [Havana]
      Squiggle
      [Spade, T, Square]
      DancingMad

granTeatroDeLaHabana :: CardDef
granTeatroDeLaHabana =
  victory 1
    $ location
      "09603"
      "Gran Teatro de la Habana"
      [Havana]
      Square
      [Hourglass, Equals, Squiggle]
      DancingMad

miramarYachtClub :: CardDef
miramarYachtClub =
  location
    "09604"
    "Miramar Yacht Club"
    [Havana]
    Equals
    [Hourglass, Square, T]
    DancingMad

plazaHotel :: CardDef
plazaHotel =
  victory 1
    $ location
      "09605"
      "Plaza Hotel"
      [Havana]
      Hourglass
      [Equals, Square]
      DancingMad

outsidersLair :: CardDef
outsidersLair =
  otherSideIs "09615"
    $ victory 1
    $ location
      "09615b"
      "Outsiders' Lair"
      [Otherworld]
      NoSymbol
      []
      OnThinIce

anchorage :: CardDef
anchorage =
  location
    "09617"
    "Anchorage"
    []
    Squiggle
    [Diamond]
    OnThinIce

fairbanks :: CardDef
fairbanks =
  location
    "09618"
    "Fairbanks"
    []
    Diamond
    [Squiggle, Triangle, Square, Circle, Moon]
    OnThinIce

mountainStream :: CardDef
mountainStream =
  locationWithUnrevealed
    "09619"
    "Alaskan Wilderness"
    [Wilderness]
    Triangle
    [Diamond]
    "Mountain Stream"
    [Wilderness]
    Square
    [Diamond, T, Spade, Heart, Hourglass]
    OnThinIce

frozenLake :: CardDef
frozenLake =
  locationWithUnrevealed
    "09620"
    "Alaskan Wilderness"
    [Wilderness]
    Triangle
    [Diamond]
    "Frozen Lake"
    [Wilderness]
    Circle
    [Diamond]
    OnThinIce

isolatedRoad :: CardDef
isolatedRoad =
  victory 1
    $ locationWithUnrevealed
      "09621"
      "Alaskan Wilderness"
      [Wilderness]
      Triangle
      [Diamond]
      "Isolated Road"
      [Wilderness]
      Moon
      [Diamond, Spade]
      OnThinIce

forgottenOutpost :: CardDef
forgottenOutpost =
  locationWithUnrevealed
    "09622"
    "Outer Wilderness"
    [Wilderness]
    T
    [Square]
    "Forgotten Outpost"
    [Wilderness]
    Spade
    [Square, Moon]
    OnThinIce

huntersLodge :: CardDef
huntersLodge =
  victory 1
    $ locationWithUnrevealed
      "09623"
      "Outer Wilderness"
      [Wilderness]
      T
      [Square]
      "Hunter's Lodge"
      [Wilderness]
      Heart
      [Square]
      OnThinIce

condemnedGoldMine :: CardDef
condemnedGoldMine =
  locationWithUnrevealed
    "09624"
    "Outer Wilderness"
    [Wilderness]
    T
    [Square]
    "Condemned Gold Mine"
    []
    Hourglass
    [Square]
    OnThinIce

theBourseLocusSafeguard :: CardDef
theBourseLocusSafeguard =
  location
    "09642"
    ("The Bourse" <:> "Locus Safeguard")
    [Alexandria]
    Square
    [Circle, Triangle]
    DogsOfWar

theBourseCoteriePost :: CardDef
theBourseCoteriePost =
  victory 1
    $ location
      "09643"
      ("The Bourse" <:> "Coterie Post")
      [Alexandria]
      Square
      [Circle, Triangle]
      DogsOfWar

theBourseCommercialCenter :: CardDef
theBourseCommercialCenter =
  location
    "09644"
    ("The Bourse" <:> "Commercial Center")
    [Alexandria]
    Square
    [Circle, Triangle]
    DogsOfWar

windsorPalaceHotel :: CardDef
windsorPalaceHotel =
  location
    "09645"
    "Windsor Palace Hotel"
    [Alexandria, LocusSite]
    Circle
    [Square, Triangle, Star]
    DogsOfWar

victoriaCollege :: CardDef
victoriaCollege =
  location
    "09646"
    "Victoria College"
    [Alexandria, LocusSite]
    Triangle
    [Square, Circle, Star]
    DogsOfWar

qaitbayCitadel :: CardDef
qaitbayCitadel =
  location
    "09647"
    "Qaitbay Citadel"
    [Alexandria]
    Star
    [Circle, Triangle, Equals, Moon]
    DogsOfWar

theCorniche :: CardDef
theCorniche =
  location
    "09648"
    "The Corniche"
    [Alexandria, LocusSite]
    Equals
    [Squiggle, Moon, Star]
    DogsOfWar

zanEtElSettat :: CardDef
zanEtElSettat =
  location
    "09649"
    "Zan'et el Settat"
    [Alexandria, LocusSite]
    Moon
    [Squiggle, Equals, Star]
    DogsOfWar

catacombsOfKomElShoqafaDenOfTheBeast :: CardDef
catacombsOfKomElShoqafaDenOfTheBeast =
  victory 1
    $ location
      "09650"
      ("Catacombs of Kom el Shoqafa" <:> "Den of the Beast")
      [Alexandria]
      Squiggle
      [Equals, Moon]
      DogsOfWar

catacombsOfKomElShoqafaBloodyNexus :: CardDef
catacombsOfKomElShoqafaBloodyNexus =
  victory 1
    $ location
      "09651"
      ("Catacombs of Kom el Shoqafa" <:> "Bloody Nexus")
      [Alexandria]
      Squiggle
      [Equals, Moon]
      DogsOfWar

catacombsOfKomElShoqafaAncientTomb :: CardDef
catacombsOfKomElShoqafaAncientTomb =
  victory 1
    $ location
      "09652"
      ("Catacombs of Kom el Shoqafa" <:> "Ancient Tomb")
      [Alexandria]
      Squiggle
      [Equals, Moon]
      DogsOfWar

kualaLumpurStationEastWing :: CardDef
kualaLumpurStationEastWing =
  location
    "09667"
    ("Kuala Lumpur Station" <:> "East Wing")
    [KualaLumpur]
    Diamond
    [Moon, Star, Triangle, Squiggle]
    ShadesOfSuffering

kualaLumpurStationWestWing :: CardDef
kualaLumpurStationWestWing =
  victory 1
    $ location
      "09668"
      ("Kuala Lumpur Station" <:> "West Wing")
      [KualaLumpur, Haunted]
      Triangle
      [Diamond, Circle, Square]
      ShadesOfSuffering

selangorClub :: CardDef
selangorClub =
  location
    "09669"
    "Selangor Club"
    [KualaLumpur]
    Circle
    [Triangle, Square]
    ShadesOfSuffering

selangorClubPadang :: CardDef
selangorClubPadang =
  victory 1
    $ location
      "09670"
      "Selangor Club Padang"
      [KualaLumpur]
      Square
      [Triangle, Circle]
      ShadesOfSuffering

melatisShop :: CardDef
melatisShop =
  location
    "09671"
    "Melati's Shop"
    [KualaLumpur]
    Squiggle
    [Diamond]
    ShadesOfSuffering

wayangKulitTheater :: CardDef
wayangKulitTheater =
  victory 1
    $ location
      "09672"
      "Wayang Kulit Theater"
      [KualaLumpur]
      Moon
      [Diamond]
      ShadesOfSuffering

tinMine :: CardDef
tinMine =
  victory 1
    $ location
      "09673"
      "Tin Mine"
      [KualaLumpur]
      Star
      [Diamond]
      ShadesOfSuffering

courtOfTheOutsiders :: CardDef
courtOfTheOutsiders =
  location_
    "09688"
    "Court of the Outsiders"
    [Otherworld]
    WithoutATrace

outsidersLairWithoutATrace :: CardDef
outsidersLairWithoutATrace =
  victory 1
    $ ( locationWithUnrevealed_
          "09689"
          "City of Remnants"
          [Otherworld]
          "Outsiders' Lair"
          [Otherworld]
          WithoutATrace
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      }

scarletHallsSanctum :: CardDef
scarletHallsSanctum =
  location
    "09704"
    "Scarlet Halls"
    [Sanctum]
    Squiggle
    [Square, Triangle, Diamond, Circle]
    CongressOfTheKeys

scarletHallsLair :: CardDef
scarletHallsLair =
  location
    "09705"
    "Scarlet Halls"
    [Lair]
    Squiggle
    [Square, Triangle, Diamond, Circle]
    CongressOfTheKeys

congressChamberSanctum :: CardDef
congressChamberSanctum =
  locationWithUnrevealed
    "09706"
    "Coterie Sanctuary"
    [Sanctum]
    Circle
    [Squiggle]
    "Congress Chamber"
    [Sanctum]
    Square
    [Squiggle]
    CongressOfTheKeys

congressChamberLair :: CardDef
congressChamberLair =
  locationWithUnrevealed
    "09707"
    "Coterie Sanctuary"
    [Lair]
    Circle
    [Squiggle]
    "Congress Chamber"
    [Lair]
    Square
    [Squiggle]
    CongressOfTheKeys

coterieLibrarySanctum :: CardDef
coterieLibrarySanctum =
  locationWithUnrevealed
    "09708"
    "Coterie Sanctuary"
    [Sanctum]
    Circle
    [Squiggle]
    "Coterie Library"
    [Sanctum]
    Triangle
    [Squiggle]
    CongressOfTheKeys

coterieLibraryLair :: CardDef
coterieLibraryLair =
  locationWithUnrevealed
    "09709"
    "Coterie Sanctuary"
    [Lair]
    Circle
    [Squiggle]
    "Coterie Library"
    [Lair]
    Triangle
    [Squiggle]
    CongressOfTheKeys

theKeyReliquarySanctum :: CardDef
theKeyReliquarySanctum =
  locationWithUnrevealed
    "09710"
    "Coterie Sanctuary"
    [Sanctum]
    Circle
    [Squiggle]
    "The Key Reliquary"
    [Sanctum]
    Diamond
    [Squiggle]
    CongressOfTheKeys

theKeyReliquaryLair :: CardDef
theKeyReliquaryLair =
  locationWithUnrevealed
    "09711"
    "Coterie Sanctuary"
    [Lair]
    Circle
    [Squiggle]
    "The Key Reliquary"
    [Lair]
    Diamond
    [Squiggle]
    CongressOfTheKeys

theKnottedTower :: CardDef
theKnottedTower =
  location
    "09712"
    "The Knotted Tower"
    [Otherworld]
    Moon
    [Equals]
    CongressOfTheKeys

gravityDefyingClimb :: CardDef
gravityDefyingClimb =
  location
    "09713"
    "Gravity-Defying Climb"
    [Otherworld, Tower]
    Equals
    [Moon, Star]
    CongressOfTheKeys

theToweringVertexRuinousConflux :: CardDef
theToweringVertexRuinousConflux =
  location
    "09714"
    ("The Towering Vertex" <:> "Ruinous Conflux")
    [Otherworld, Tower]
    Star
    [Equals]
    CongressOfTheKeys

ramblingRouteA :: CardDef
ramblingRouteA =
  ( locationWithUnrevealed_
      "09747a"
      "City of Remnants"
      [Otherworld]
      "Rambling Route"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

ramblingRouteB :: CardDef
ramblingRouteB =
  ( locationWithUnrevealed_
      "09747b"
      "City of Remnants"
      [Otherworld]
      "Rambling Route"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

ramblingRouteC :: CardDef
ramblingRouteC =
  ( locationWithUnrevealed_
      "09747c"
      "City of Remnants"
      [Otherworld]
      "Rambling Route"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

alienFrontierA :: CardDef
alienFrontierA =
  ( locationWithUnrevealed_
      "09748a"
      "City of Remnants"
      [Otherworld]
      "Alien Frontier"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

alienFrontierB :: CardDef
alienFrontierB =
  ( locationWithUnrevealed_
      "09748b"
      "City of Remnants"
      [Otherworld]
      "Alien Frontier"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

wealdOfEffigiesA :: CardDef
wealdOfEffigiesA =
  victory 1
    $ ( locationWithUnrevealed_
          "09749a"
          "City of Remnants"
          [Otherworld]
          "Weald of Effigies"
          [Otherworld]
          BeyondTheBeyond
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      }

wealdOfEffigiesB :: CardDef
wealdOfEffigiesB =
  victory 1
    $ ( locationWithUnrevealed_
          "09749b"
          "City of Remnants"
          [Otherworld]
          "Weald of Effigies"
          [Otherworld]
          BeyondTheBeyond
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      }

cliffsOfInsanity :: CardDef
cliffsOfInsanity =
  victory 1
    $ ( locationWithUnrevealed_
          "09750"
          "City of Remnants"
          [Otherworld]
          "Cliffs of Insanity"
          [Otherworld]
          BeyondTheBeyond
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      , cdEncounterSetQuantity = Just 2
      }
