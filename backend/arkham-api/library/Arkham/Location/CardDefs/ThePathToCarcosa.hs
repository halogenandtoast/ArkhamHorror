{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.ThePathToCarcosa where

import Arkham.Location.CardDefs.Import

theatre :: CardDef
theatre =
  location "03049" "Theatre" mempty Circle [Diamond, Triangle] CurtainCall

lobby :: CardDef
lobby =
  location "03050" "Lobby" mempty Triangle [Circle, Square, Plus] CurtainCall

balcony :: CardDef
balcony =
  victory 1
    $ location "03051" "Balcony" mempty Square [Circle, Triangle] CurtainCall

backstage :: CardDef
backstage =
  location "03052" "Backstage" mempty Diamond [Circle, Moon] CurtainCall

lightingBox :: CardDef
lightingBox =
  victory 1
    $ locationWithUnrevealed
      "03053"
      "Lobby Doorway"
      [Private]
      Plus
      [Triangle]
      "Lighting Box"
      [Private]
      Plus
      [Triangle]
      CurtainCall

boxOffice :: CardDef
boxOffice =
  locationWithUnrevealed
    "03054"
    "Lobby Doorway"
    [Private]
    Plus
    [Triangle]
    "Box Office"
    [Private]
    Plus
    [Triangle]
    CurtainCall

greenRoom :: CardDef
greenRoom =
  victory 1
    $ locationWithUnrevealed
      "03055"
      "Lobby Doorway"
      [Private]
      Plus
      [Triangle]
      "Green Room"
      [Private]
      Plus
      [Triangle]
      CurtainCall

dressingRoom :: CardDef
dressingRoom =
  locationWithUnrevealed
    "03056"
    "Backstage Doorway"
    [Private]
    Moon
    [Diamond]
    "Dressing Room"
    [Private]
    Moon
    [Diamond]
    CurtainCall

rehearsalRoom :: CardDef
rehearsalRoom =
  victory 1
    $ locationWithUnrevealed
      "03057"
      "Backstage Doorway"
      [Private]
      Moon
      [Diamond]
      "Rehearsal Room"
      [Private]
      Moon
      [Diamond]
      CurtainCall

trapRoom :: CardDef
trapRoom =
  victory 1
    $ locationWithUnrevealed
      "03058"
      "Backstage Doorway"
      [Private]
      Moon
      [Diamond]
      "Trap Room"
      [Private]
      Moon
      [Diamond]
      CurtainCall

foyer :: CardDef
foyer = location "03070" "Foyer" mempty T [Circle, Square, Equals] TheLastKing

ballroom :: CardDef
ballroom =
  location "03071" "Ballroom" mempty Square [T, Circle, Squiggle] TheLastKing

livingRoom :: CardDef
livingRoom =
  location "03072" "Living Room" mempty Equals [T, Circle, Plus] TheLastKing

gallery :: CardDef
gallery = location "03073" "Gallery" mempty Plus [Equals, Circle] TheLastKing

courtyard :: CardDef
courtyard =
  location
    "03074"
    "Courtyard"
    mempty
    Circle
    [Squiggle, Square, T, Equals, Plus]
    TheLastKing

diningRoom :: CardDef
diningRoom =
  location "03075" "Dining Room" mempty Squiggle [Square, Circle] TheLastKing

entryHall :: CardDef
entryHall =
  location "03127" "Entry Hall" [GroundFloor] Square [Circle] EchoesOfThePast

historicalSocietyMeetingRoom :: CardDef
historicalSocietyMeetingRoom =
  locationWithUnrevealed
    "03128"
    "Historical Society"
    [GroundFloor]
    NoSymbol
    [Square]
    ("Historical Society" <:> "Meeting Room")
    [GroundFloor, Passageway]
    Diamond
    [Square]
    EchoesOfThePast

historicalSocietyRecordOffice_129 :: CardDef
historicalSocietyRecordOffice_129 =
  locationWithUnrevealed
    "03129"
    "Historical Society"
    [GroundFloor]
    NoSymbol
    [Square]
    ("Historical Society" <:> "Record Office")
    [GroundFloor]
    Plus
    [Square]
    EchoesOfThePast

historicalSocietyHistoricalMuseum_130 :: CardDef
historicalSocietyHistoricalMuseum_130 =
  locationWithUnrevealed
    "03130"
    "Historical Society"
    [GroundFloor]
    NoSymbol
    [Square]
    ("Historical Society" <:> "Historical Museum")
    [GroundFloor]
    Heart
    [Square, Hourglass]
    EchoesOfThePast

quietHalls_131 :: CardDef
quietHalls_131 =
  location
    "03131"
    "Quiet Halls"
    [SecondFloor]
    Circle
    [Square, Star]
    EchoesOfThePast

historicalSocietyHistoricalMuseum_132 :: CardDef
historicalSocietyHistoricalMuseum_132 =
  locationWithUnrevealed
    "03132"
    "Historical Society"
    [SecondFloor]
    NoSymbol
    [Circle]
    ("Historical Society" <:> "Historical Museum")
    [SecondFloor]
    Hourglass
    [Circle, Heart]
    EchoesOfThePast

historicalSocietyHistoricalLibrary_133 :: CardDef
historicalSocietyHistoricalLibrary_133 =
  locationWithUnrevealed
    "03133"
    "Historical Society"
    [SecondFloor]
    NoSymbol
    [Circle]
    ("Historical Society" <:> "Historical Library")
    [SecondFloor, Passageway]
    Triangle
    [Circle, Squiggle]
    EchoesOfThePast

historicalSocietyReadingRoom :: CardDef
historicalSocietyReadingRoom =
  locationWithUnrevealed
    "03134"
    "Historical Society"
    [SecondFloor]
    NoSymbol
    [Circle]
    ("Historical Society" <:> "Reading Room")
    [SecondFloor]
    T
    [Circle]
    EchoesOfThePast

quietHalls_135 :: CardDef
quietHalls_135 =
  location "03135" "Quiet Halls" [ThirdFloor] Star [Circle] EchoesOfThePast

historicalSocietyHistoricalLibrary_136 :: CardDef
historicalSocietyHistoricalLibrary_136 =
  locationWithUnrevealed
    "03136"
    "Historical Society"
    [ThirdFloor]
    NoSymbol
    [Star]
    ("Historical Society" <:> "Historical Library")
    [ThirdFloor, Passageway]
    Squiggle
    [Star, Triangle]
    EchoesOfThePast

historicalSocietyPeabodysOffice :: CardDef
historicalSocietyPeabodysOffice =
  locationWithUnrevealed
    "03137"
    "Historical Society"
    [ThirdFloor]
    NoSymbol
    [Star]
    ("Historical Society" <:> "Peabody's Office")
    [ThirdFloor, Passageway]
    Moon
    [Star]
    EchoesOfThePast

historicalSocietyRecordOffice_138 :: CardDef
historicalSocietyRecordOffice_138 =
  locationWithUnrevealed
    "03138"
    "Historical Society"
    [ThirdFloor]
    NoSymbol
    [Star]
    ("Historical Society" <:> "Record Office")
    [ThirdFloor]
    Equals
    [Star]
    EchoesOfThePast

hiddenLibrary :: CardDef
hiddenLibrary =
  victory 2
    $ location "03139" "Hidden Library" mempty NoSymbol [] EchoesOfThePast

asylumHallsWesternPatientWing_168 :: CardDef
asylumHallsWesternPatientWing_168 =
  location
    "03168"
    ("Asylum Halls" <:> "Western Patient Wing")
    [ArkhamAsylum]
    Circle
    [Hourglass, Triangle, Diamond]
    TheUnspeakableOath

asylumHallsWesternPatientWing_169 :: CardDef
asylumHallsWesternPatientWing_169 =
  location
    "03169"
    ("Asylum Halls" <:> "Western Patient Wing")
    [ArkhamAsylum]
    Circle
    [Hourglass, Triangle, Diamond]
    TheUnspeakableOath

asylumHallsEasternPatientWing_170 :: CardDef
asylumHallsEasternPatientWing_170 =
  location
    "03170"
    ("Asylum Halls" <:> "Eastern Patient Wing")
    [ArkhamAsylum]
    Hourglass
    [Circle, Heart, Squiggle]
    TheUnspeakableOath

asylumHallsEasternPatientWing_171 :: CardDef
asylumHallsEasternPatientWing_171 =
  location
    "03171"
    ("Asylum Halls" <:> "Eastern Patient Wing")
    [ArkhamAsylum]
    Hourglass
    [Circle, Heart, Squiggle]
    TheUnspeakableOath

kitchen :: CardDef
kitchen =
  location "03172" "Kitchen" [ArkhamAsylum] Square [Triangle] TheUnspeakableOath

messHall :: CardDef
messHall =
  victory 1
    $ location
      "03173"
      "Mess Hall"
      [ArkhamAsylum]
      Triangle
      [Circle, Square]
      TheUnspeakableOath

infirmary :: CardDef
infirmary =
  victory 1
    $ location
      "03174"
      "Infirmary"
      [ArkhamAsylum]
      Heart
      [Hourglass]
      TheUnspeakableOath

yard :: CardDef
yard =
  location
    "03175"
    "Yard"
    [ArkhamAsylum]
    Diamond
    [Circle, Plus]
    TheUnspeakableOath

garden :: CardDef
garden =
  location "03176" "Garden" [ArkhamAsylum] Plus [Diamond] TheUnspeakableOath

basementHall :: CardDef
basementHall =
  victory 1
    $ location
      "03177"
      "Basement Hall"
      [ArkhamAsylum]
      Squiggle
      [Hourglass, Moon]
      TheUnspeakableOath

patientConfinementDanielsCell :: CardDef
patientConfinementDanielsCell =
  locationWithUnrevealed
    "03178"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Daniel's Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

patientConfinementOccupiedCell :: CardDef
patientConfinementOccupiedCell =
  locationWithUnrevealed
    "03179"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Occupied Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

patientConfinementDrearyCell :: CardDef
patientConfinementDrearyCell =
  locationWithUnrevealed
    "03180"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Dreary Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

patientConfinementFamiliarCell :: CardDef
patientConfinementFamiliarCell =
  locationWithUnrevealed
    "03181"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Familiar Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

montparnasse :: CardDef
montparnasse =
  location
    "03208"
    "Montparnasse"
    [Paris, Rail]
    Circle
    [Heart, Star, Plus]
    APhantomOfTruth

montmartre209 :: CardDef
montmartre209 =
  location
    "03209"
    "Montmartre"
    [Paris, Rail]
    Square
    [Diamond, Triangle, Equals, Moon]
    APhantomOfTruth

montmartre210 :: CardDef
montmartre210 =
  location
    "03210"
    "Montmartre"
    [Paris, Rail]
    Square
    [Diamond, Triangle, Equals, Moon]
    APhantomOfTruth

grandGuignol :: CardDef
grandGuignol =
  victory 1
    $ location
      "03211"
      ("Grand Guignol" <:> "Theatre of the Great Puppet")
      [Paris]
      Triangle
      [Diamond, Square]
      APhantomOfTruth

operaGarnier212 :: CardDef
operaGarnier212 =
  location
    "03212"
    "Opéra Garnier"
    [Paris, Rail]
    Diamond
    [Triangle, Square, Heart]
    APhantomOfTruth

operaGarnier213 :: CardDef
operaGarnier213 =
  location
    "03213"
    "Opéra Garnier"
    [Paris, Rail]
    Diamond
    [Triangle, Square, Heart]
    APhantomOfTruth

gareDOrsay :: CardDef
gareDOrsay =
  location
    "03214"
    "Gare d'Orsay"
    [Paris, Rail]
    Heart
    [Diamond, Circle, Star]
    APhantomOfTruth

pereLachaiseCemetery :: CardDef
pereLachaiseCemetery =
  victory 1
    $ location
      "03215"
      "Père Lachaise Cemetery"
      [Paris]
      T
      [Equals, Moon]
      APhantomOfTruth

canalSaintMartin :: CardDef
canalSaintMartin =
  victory 1
    $ location
      "03216"
      "Canal Saint-Martin"
      [Paris]
      Equals
      [Square, T, Moon]
      APhantomOfTruth

leMarais217 :: CardDef
leMarais217 =
  location
    "03217"
    "Le Marais"
    [Paris, Rail]
    Moon
    [Square, Equals, T, Plus]
    APhantomOfTruth

leMarais218 :: CardDef
leMarais218 =
  location
    "03218"
    "Le Marais"
    [Paris, Rail]
    Moon
    [Square, Equals, T, Plus]
    APhantomOfTruth

notreDame :: CardDef
notreDame =
  location
    "03219"
    "Notre-Dame"
    [Paris, Rail]
    Plus
    [Circle, Moon, Star]
    APhantomOfTruth

gardensOfLuxembourg :: CardDef
gardensOfLuxembourg =
  victory 1
    $ location
      "03220"
      "Gardens of Luxembourg"
      [Paris]
      Star
      [Circle, Heart, Plus]
      APhantomOfTruth

theGateToHell :: CardDef
theGateToHell =
  locationWithUnrevealed
    "03247"
    "Catacombs"
    []
    NoSymbol
    []
    "The Gate to Hell"
    []
    NoSymbol
    []
    ThePallidMask

stoneArchways :: CardDef
stoneArchways =
  quantity 2
    $ locationWithUnrevealed
      "03248"
      "Catacombs"
      []
      NoSymbol
      []
      "Stone Archways"
      []
      NoSymbol
      []
      ThePallidMask

cryptOfTheSepulchralLamp :: CardDef
cryptOfTheSepulchralLamp =
  locationWithUnrevealed
    "03249"
    "Catacombs"
    []
    NoSymbol
    []
    "Crypt of the Sepulchral Lamp"
    []
    NoSymbol
    []
    ThePallidMask

boneFilledCaverns :: CardDef
boneFilledCaverns =
  victory 1
    $ locationWithUnrevealed
      "03250"
      "Catacombs"
      []
      NoSymbol
      []
      "Bone-Filled Caverns"
      []
      NoSymbol
      []
      ThePallidMask

wellOfSouls :: CardDef
wellOfSouls =
  victory 1
    $ locationWithUnrevealed
      "03251"
      "Catacombs"
      []
      NoSymbol
      []
      "Well of Souls"
      []
      NoSymbol
      []
      ThePallidMask

candlelitTunnels :: CardDef
candlelitTunnels =
  quantity 2
    $ locationWithUnrevealed
      "03252"
      "Catacombs"
      []
      NoSymbol
      []
      "Candlelit Tunnels"
      []
      NoSymbol
      []
      ThePallidMask

labyrinthOfBones :: CardDef
labyrinthOfBones =
  quantity 2
    $ locationWithUnrevealed
      "03253"
      "Catacombs"
      []
      NoSymbol
      []
      "Labyrinth of Bones"
      []
      NoSymbol
      []
      ThePallidMask

narrowShaft :: CardDef
narrowShaft =
  victory 1
    $ locationWithUnrevealed
      "03254"
      "Catacombs"
      []
      NoSymbol
      []
      "Narrow Shaft"
      []
      NoSymbol
      []
      ThePallidMask

shiveringPools :: CardDef
shiveringPools =
  victory 1
    $ locationWithUnrevealed
      "03255"
      "Catacombs"
      []
      NoSymbol
      []
      "Shivering Pools"
      []
      NoSymbol
      []
      ThePallidMask

blockedPassage :: CardDef
blockedPassage =
  locationWithUnrevealed
    "03256"
    "Catacombs"
    []
    NoSymbol
    []
    "Blocked Passage"
    []
    NoSymbol
    []
    ThePallidMask

tombOfShadows :: CardDef
tombOfShadows =
  victory 1
    $ locationWithUnrevealed
      "03257"
      "Catacombs"
      []
      NoSymbol
      []
      "Tomb of Shadows"
      []
      NoSymbol
      []
      ThePallidMask

porteDeLAvancee :: CardDef
porteDeLAvancee =
  location "03283" "Porte de l'Avancée" [] Circle [Squiggle] BlackStarsRise

grandRue :: CardDef
grandRue =
  location
    "03284"
    "Grand Rue"
    []
    Squiggle
    [Circle, Triangle, Diamond, Equals]
    BlackStarsRise

outerWall_285 :: CardDef
outerWall_285 =
  victory 1
    $ location
      "03285"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

outerWall_286 :: CardDef
outerWall_286 =
  victory 1
    $ location
      "03286"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

northTower_287 :: CardDef
northTower_287 =
  victory 1
    $ location
      "03287"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

northTower_288 :: CardDef
northTower_288 =
  victory 1
    $ location
      "03288"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

brokenSteps_289 :: CardDef
brokenSteps_289 =
  location
    "03289"
    "Broken Steps"
    []
    Equals
    [Squiggle, Triangle, Diamond, Square]
    BlackStarsRise

brokenSteps_290 :: CardDef
brokenSteps_290 =
  location
    "03290"
    "Broken Steps"
    []
    Equals
    [Squiggle, Triangle, Diamond, Square]
    BlackStarsRise

abbeyChurch :: CardDef
abbeyChurch =
  victory 1
    $ location
      "03291"
      "Abbey Church"
      []
      Square
      [Equals, T, Heart, Hourglass, Moon]
      BlackStarsRise

choeurGothique_292 :: CardDef
choeurGothique_292 =
  location "03292" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

choeurGothique_293 :: CardDef
choeurGothique_293 =
  location "03293" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

cloister :: CardDef
cloister =
  location "03294" "Cloister" [] Heart [Square, Hourglass] BlackStarsRise

knightsHall :: CardDef
knightsHall =
  location "03295" "Knight's Hall" [] Hourglass [Square, Heart] BlackStarsRise

chapelOfStAubertThePathIsOpen :: CardDef
chapelOfStAubertThePathIsOpen =
  locationWithUnrevealed
    "03296"
    "Chapel of St. Aubert"
    []
    Moon
    [Square]
    ("Chapel of St. Aubert" <:> "The Path is Open")
    []
    Moon
    [Square]
    BlackStarsRise

chapelOfStAubertWatersForbidden :: CardDef
chapelOfStAubertWatersForbidden =
  victory 2
    $ locationWithUnrevealed
      "03297"
      "Chapel of St. Aubert"
      []
      Moon
      [Square]
      ("Chapel of St. Aubert" <:> "Waters Forbidden")
      []
      Moon
      [Square]
      BlackStarsRise

abbeyTowerThePathIsOpen :: CardDef
abbeyTowerThePathIsOpen =
  locationWithUnrevealed
    "03298"
    "Abbey Tower"
    []
    Star
    [T]
    ("Abbey Tower" <:> "The Path is Open")
    []
    Star
    [T]
    BlackStarsRise

abbeyTowerSpiresForbidden :: CardDef
abbeyTowerSpiresForbidden =
  victory 2
    $ locationWithUnrevealed
      "03299"
      "Abbey Tower"
      []
      Star
      [T]
      ("Abbey Tower" <:> "Spires Forbidden")
      []
      Star
      [T]
      BlackStarsRise

shoresOfHali :: CardDef
shoresOfHali =
  storyOnBack' "03325b" $ location "03325a" "Shores of Hali" [Otherworld] Circle [Square] DimCarcosa

bleakPlainsStarsOfAldebaran :: CardDef
bleakPlainsStarsOfAldebaran =
  storyOnBack' "03326b"
    $ location
      "03326a"
      "Bleak Plains"
      [Otherworld]
      Square
      [Circle, Triangle, Diamond]
      DimCarcosa

bleakPlainsBleakDesolation :: CardDef
bleakPlainsBleakDesolation =
  storyOnBack' "03326d"
    $ location
      "03326c"
      "Bleak Plains"
      [Otherworld]
      Square
      [Circle, Triangle, Diamond]
      DimCarcosa

ruinsOfCarcosaInhabitantOfCarcosa :: CardDef
ruinsOfCarcosaInhabitantOfCarcosa =
  storyOnBack' "03327b"
    $ location
      "03327a"
      "Ruins of Carcosa"
      [Otherworld]
      Triangle
      [Square, Equals, Star]
      DimCarcosa

ruinsOfCarcosaAMomentsRest :: CardDef
ruinsOfCarcosaAMomentsRest =
  storyOnBack' "03327d"
    $ location
      "03327c"
      "Ruins of Carcosa"
      [Otherworld]
      Triangle
      [Square, Equals, Star]
      DimCarcosa

ruinsOfCarcosaTheCoffin :: CardDef
ruinsOfCarcosaTheCoffin =
  storyOnBack' "03327f"
    $ location
      "03327e"
      "Ruins of Carcosa"
      [Otherworld]
      Triangle
      [Square, Equals, Star]
      DimCarcosa

dimStreetsMappingTheStreets :: CardDef
dimStreetsMappingTheStreets =
  storyOnBack' "03328b"
    $ location
      "03328a"
      "Dim Streets"
      [Otherworld]
      Diamond
      [Square, Equals, Star]
      DimCarcosa

dimStreetsTheKingsParade :: CardDef
dimStreetsTheKingsParade =
  storyOnBack' "03328d"
    $ location
      "03328c"
      "Dim Streets"
      [Otherworld]
      Diamond
      [Square, Equals, Star]
      DimCarcosa

dimStreetsTheArchway :: CardDef
dimStreetsTheArchway =
  storyOnBack' "03328f"
    $ location
      "03328e"
      "Dim Streets"
      [Otherworld]
      Diamond
      [Square, Equals, Star]
      DimCarcosa

depthsOfDemheTheHeightOfTheDepths :: CardDef
depthsOfDemheTheHeightOfTheDepths =
  storyOnBack' "03329b"
    $ location
      "03329a"
      "Depths of Demhe"
      [Otherworld]
      Equals
      [Moon, Triangle, Diamond]
      DimCarcosa

depthsOfDemheStepsOfThePalace :: CardDef
depthsOfDemheStepsOfThePalace =
  storyOnBack' "03329d"
    $ location
      "03329c"
      "Depths of Demhe"
      [Otherworld]
      Equals
      [Moon, Triangle, Diamond]
      DimCarcosa

darkSpires :: CardDef
darkSpires =
  storyOnBack' "03330b"
    $ location "03330" "Dark Spires" [Otherworld] Moon [Equals] DimCarcosa

palaceOfTheKing :: CardDef
palaceOfTheKing =
  storyOnBack' "03331b"
    $ location "03331" "Palace of the King" [Otherworld] Star [Triangle, Diamond] DimCarcosa
