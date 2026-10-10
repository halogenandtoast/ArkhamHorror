module Arkham.Homebrew.AgesUnwound.CardDefs.Locations where

import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Homebrew.AgesUnwound.Traits
import Arkham.Location.CardDefs.Import

rivertown :: CardDef
rivertown =
  locationWithUnrevealed
    ":ages-unwound:008"
    "Arkham Streets"
    [Arkham]
    Circle
    [Square, Moon, Diamond]
    ("Rivertown" <:> "Your Home in Flames")
    [Arkham, Central]
    Circle
    [Square, Moon, Diamond]
    Set.NightOfFire

independenceSquare :: CardDef
independenceSquare =
  locationWithUnrevealed
    ":ages-unwound:009"
    "Arkham Streets"
    [Arkham]
    Square
    [Circle, Moon, T, Diamond, Hourglass, Triangle]
    ("Independence Square" <:> "Out in the Open")
    [Arkham]
    Square
    [Circle, Moon, T, Diamond, Hourglass, Triangle]
    Set.NightOfFire

easttown :: CardDef
easttown =
  locationWithUnrevealed
    ":ages-unwound:010"
    "Arkham Streets"
    [Arkham]
    Moon
    [Circle, Square, Triangle]
    ("Easttown" <:> "North of the River")
    [Arkham]
    Moon
    [Circle, Square, Triangle]
    Set.NightOfFire

frenchHill :: CardDef
frenchHill =
  victory 1
    $ locationWithUnrevealed
      ":ages-unwound:011"
      "Arkham Streets"
      [Arkham]
      T
      [Square, Hourglass, Triangle]
      ("French Hill" <:> "Fog Swirls and Obscures")
      [Arkham]
      T
      [Square, Hourglass, Triangle]
      Set.NightOfFire

twistingAlleys :: CardDef
twistingAlleys =
  locationWithUnrevealed
    ":ages-unwound:012"
    "Arkham Streets"
    [Arkham]
    Diamond
    [Circle, Square, Hourglass, Plus]
    ("Twisting Alleys" <:> "A Rat in a Maze")
    [Arkham]
    Diamond
    [Circle, Square, Hourglass, Plus]
    Set.NightOfFire

winchmoreHouse :: CardDef
winchmoreHouse =
  locationWithUnrevealed
    ":ages-unwound:013"
    "Arkham Streets"
    [Arkham]
    Hourglass
    [Square, T, Diamond]
    ("Winchmore House" <:> "Perfect for an Ambush")
    [Arkham]
    Hourglass
    [Square, T, Diamond]
    Set.NightOfFire

crampedPassage :: CardDef
crampedPassage =
  locationWithUnrevealed
    ":ages-unwound:014"
    "Arkham Streets"
    [Arkham]
    Triangle
    [Square, Moon, T]
    ("Cramped Passage" <:> "Tight Fit")
    [Arkham]
    Triangle
    [Square, Moon, T]
    Set.NightOfFire

aPlaceToHide :: CardDef
aPlaceToHide =
  locationWithUnrevealed
    ":ages-unwound:015"
    "Arkham Streets"
    [Arkham]
    Plus
    [Diamond]
    ("A Place to Hide" <:> "Rotting Sanctuary")
    [Arkham]
    Plus
    [Diamond]
    Set.NightOfFire

lawn :: CardDef
lawn =
  location
    ":ages-unwound:030"
    "Lawn"
    [Garden]
    Circle
    [Diamond, Triangle, Square, Squiggle]
    Set.TheMyriadGentleman

hedgeMaze :: CardDef
hedgeMaze =
  victory 1
    $ location
      ":ages-unwound:031"
      "Hedge Maze"
      [Garden]
      Diamond
      [Circle, Square, Squiggle]
      Set.TheMyriadGentleman

ornateFountain :: CardDef
ornateFountain =
  location
    ":ages-unwound:032"
    "Ornate Fountain"
    [Garden]
    Square
    [Circle, Diamond, Triangle]
    Set.TheMyriadGentleman

stables :: CardDef
stables =
  location
    ":ages-unwound:033"
    "Stables"
    [Garden]
    Triangle
    [Circle, Square]
    Set.TheMyriadGentleman

entranceHall :: CardDef
entranceHall =
  location
    ":ages-unwound:034"
    "Entrance Hall"
    [Manor]
    Squiggle
    [Circle, Diamond, Equals, Moon, Hourglass]
    Set.TheMyriadGentleman

parlor :: CardDef
parlor =
  victory 1
    $ location
      ":ages-unwound:035"
      "Parlor"
      [Manor]
      Equals
      [Squiggle, Moon]
      Set.TheMyriadGentleman

kitchen :: CardDef
kitchen =
  victory 1
    $ location
      ":ages-unwound:036"
      "Kitchen"
      [Manor]
      Moon
      [Squiggle, Equals]
      Set.TheMyriadGentleman

landing :: CardDef
landing =
  location
    ":ages-unwound:037"
    "Landing"
    [Manor]
    Hourglass
    [Squiggle, Heart, Star]
    Set.TheMyriadGentleman

masterBedroom :: CardDef
masterBedroom =
  victory 1
    $ location
      ":ages-unwound:038"
      "Master Bedroom"
      [Manor]
      Heart
      [Hourglass]
      Set.TheMyriadGentleman

study :: CardDef
study =
  victory 1
    $ location
      ":ages-unwound:039"
      "Study"
      [Manor]
      Star
      [Hourglass]
      Set.TheMyriadGentleman

frontGates_056 :: CardDef
frontGates_056 =
  location
    ":ages-unwound:056"
    "Front Gates"
    [Exterior]
    Circle
    [Square, Triangle]
    Set.AWorldTornDown

sideBuilding_057 :: CardDef
sideBuilding_057 =
  location
    ":ages-unwound:057"
    "Side Building"
    [Exterior]
    Square
    [Circle, Plus, Diamond]
    Set.AWorldTornDown

childrensPlayground_058 :: CardDef
childrensPlayground_058 =
  victory 1
    $ location
      ":ages-unwound:058"
      "Children's Playground"
      [Exterior]
      Triangle
      [Circle, Plus, Squiggle]
      Set.AWorldTornDown

sportsField_059 :: CardDef
sportsField_059 =
  location
    ":ages-unwound:059"
    "Sports Field"
    [Exterior]
    Plus
    [Square, Triangle]
    Set.AWorldTornDown

aDisquietingFuture_069 :: CardDef
aDisquietingFuture_069 =
  victory 1
    $ location
      ":ages-unwound:069"
      "A Disquieting Future"
      [Adrift, Arkham]
      Diamond
      []
      Set.Unstuck

aDisquietingFuture_070 :: CardDef
aDisquietingFuture_070 =
  victory 1
    $ location
      ":ages-unwound:070"
      "A Disquieting Future"
      [Adrift, Arkham]
      Diamond
      []
      Set.Unstuck

aWorldAtWar_071 :: CardDef
aWorldAtWar_071 =
  victory 1
    $ location
      ":ages-unwound:071"
      "A World at War"
      [Adrift, France]
      Diamond
      []
      Set.Unstuck

aWorldAtWar_072 :: CardDef
aWorldAtWar_072 =
  victory 1
    $ location
      ":ages-unwound:072"
      "A World at War"
      [Adrift, France]
      Diamond
      []
      Set.Unstuck

anEarthLongDead_073 :: CardDef
anEarthLongDead_073 =
  location
    ":ages-unwound:073"
    "An Earth Long Dead"
    [Adrift]
    Diamond
    []
    Set.Unstuck

anEarthLongDead_074 :: CardDef
anEarthLongDead_074 =
  location
    ":ages-unwound:074"
    "An Earth Long Dead"
    [Adrift]
    Diamond
    []
    Set.Unstuck

arkhamMassachusetts_075 :: CardDef
arkhamMassachusetts_075 =
  victory 1
    $ location
      ":ages-unwound:075"
      ("Arkham, Massachusetts" <:> "16th Century")
      [Adrift, Arkham]
      Diamond
      []
      Set.Unstuck

arkhamMassachusetts_076 :: CardDef
arkhamMassachusetts_076 =
  victory 1
    $ location
      ":ages-unwound:076"
      ("Arkham, Massachusetts" <:> "16th Century")
      [Adrift, Arkham]
      Diamond
      []
      Set.Unstuck

banksOfTheNile_077 :: CardDef
banksOfTheNile_077 =
  victory 1
    $ location
      ":ages-unwound:077"
      "Banks of the Nile"
      [Adrift, Egypt]
      Diamond
      []
      Set.Unstuck

banksOfTheNile_078 :: CardDef
banksOfTheNile_078 =
  victory 1
    $ location
      ":ages-unwound:078"
      "Banks of the Nile"
      [Adrift, Egypt]
      Diamond
      []
      Set.Unstuck

heartOfAnEmpire_079 :: CardDef
heartOfAnEmpire_079 =
  victory 1
    $ location
      ":ages-unwound:079"
      "Heart of an Empire"
      [Adrift, Rome]
      Diamond
      []
      Set.Unstuck

heartOfAnEmpire_080 :: CardDef
heartOfAnEmpire_080 =
  victory 1
    $ location
      ":ages-unwound:080"
      "Heart of an Empire"
      [Adrift, Rome]
      Diamond
      []
      Set.Unstuck

millionsOfYearsAgo_081 :: CardDef
millionsOfYearsAgo_081 =
  victory 1
    $ location
      ":ages-unwound:081"
      "Millions of Years Ago"
      [Adrift, Cretaceous]
      Diamond
      []
      Set.Unstuck

millionsOfYearsAgo_082 :: CardDef
millionsOfYearsAgo_082 =
  victory 1
    $ location
      ":ages-unwound:082"
      "Millions of Years Ago"
      [Adrift, Cretaceous]
      Diamond
      []
      Set.Unstuck

theTatterdemalion_083 :: CardDef
theTatterdemalion_083 =
  victory 1
    $ location
      ":ages-unwound:083"
      "The Tatterdemalion"
      [Adrift, Tatterdemalion]
      Diamond
      []
      Set.Unstuck

theTatterdemalion_084 :: CardDef
theTatterdemalion_084 =
  victory 1
    $ location
      ":ages-unwound:084"
      "The Tatterdemalion"
      [Adrift, Tatterdemalion]
      Diamond
      []
      Set.Unstuck

theTimestream :: CardDef
theTimestream =
  location
    ":ages-unwound:085"
    "The Timestream"
    [Otherworld]
    Square
    [Diamond, Triangle]
    Set.Unstuck

arkhamMassachusetts_086 :: CardDef
arkhamMassachusetts_086 =
  location
    ":ages-unwound:086"
    ("Arkham, Massachusetts" <:> "Present Day?")
    [Arkham]
    Triangle
    [Square]
    Set.Unstuck

arkhamMassachusetts_111 :: CardDef
arkhamMassachusetts_111 =
  location
    ":ages-unwound:111"
    "Arkham, Massachusetts"
    [Arkham, City]
    Circle
    [Diamond, Triangle]
    Set.AYearToPlan

sanFrancisco :: CardDef
sanFrancisco =
  location
    ":ages-unwound:112"
    "San Francisco"
    [City]
    Diamond
    [Circle, Triangle]
    Set.AYearToPlan

mexicoCity :: CardDef
mexicoCity =
  victory 1
    $ location
      ":ages-unwound:113"
      "Mexico City"
      [City]
      Triangle
      [Circle, Diamond]
      Set.AYearToPlan

london :: CardDef
london =
  victory 1
    $ location
      ":ages-unwound:114"
      "London"
      [City]
      Plus
      [T, Droplet]
      Set.AYearToPlan

paris :: CardDef
paris =
  location
    ":ages-unwound:115"
    "Paris"
    [City]
    T
    [Plus, Squiggle, Moon, Square]
    Set.AYearToPlan

rome :: CardDef
rome =
  victory 1
    $ location
      ":ages-unwound:116"
      "Rome"
      [City]
      Squiggle
      [T, Moon, Equals]
      Set.AYearToPlan

istanbul :: CardDef
istanbul =
  location
    ":ages-unwound:117"
    "Istanbul"
    [City]
    Moon
    [T, Squiggle, Equals, Heart, Star]
    Set.AYearToPlan

cairo :: CardDef
cairo =
  location
    ":ages-unwound:118"
    "Cairo"
    [City]
    Equals
    [Squiggle, Moon]
    Set.AYearToPlan

tunguska :: CardDef
tunguska =
  location
    ":ages-unwound:119"
    "Tunguska"
    [Wilderness]
    Heart
    [Moon, Hourglass]
    Set.AYearToPlan

himalayas :: CardDef
himalayas =
  victory 2
    $ location
      ":ages-unwound:120"
      "Himalayas"
      [Wilderness]
      Star
      [Moon, Hourglass]
      Set.AYearToPlan

shanghai :: CardDef
shanghai =
  victory 1
    $ location
      ":ages-unwound:121"
      "Shanghai"
      [City]
      Hourglass
      [Heart, Star]
      Set.AYearToPlan

sydney :: CardDef
sydney =
  location
    ":ages-unwound:122"
    "Sydney"
    [City]
    NoSymbol
    []
    Set.AYearToPlan

anotherRealm :: CardDef
anotherRealm =
  victory 1
    $ otherSideIs ":ages-unwound:142b"
    $ location
      ":ages-unwound:142"
      "Another Realm"
      [Otherworld]
      Square
      []
      Set.Missions

theBritishLibrary :: CardDef
theBritishLibrary =
  otherSideIs ":ages-unwound:143b"
    $ location
      ":ages-unwound:143"
      ("The British Library" <:> "Knowledge Unending")
      [Library]
      Droplet
      [Plus]
      Set.Missions

featurelessStreets :: CardDef
featurelessStreets =
  location
    ":ages-unwound:168"
    "Featureless Streets"
    [Arkham]
    NoSymbol
    []
    Set.AWorldTornDownAgain

frontGates_169 :: CardDef
frontGates_169 =
  location
    ":ages-unwound:169"
    "Front Gates"
    [Exterior]
    Circle
    [Square, Triangle]
    Set.AWorldTornDownAgain

sideBuilding_170 :: CardDef
sideBuilding_170 =
  location
    ":ages-unwound:170"
    "Side Building"
    [Exterior]
    Square
    [Circle, Plus, Diamond]
    Set.AWorldTornDownAgain

childrensPlayground_171 :: CardDef
childrensPlayground_171 =
  victory 1
    $ location
      ":ages-unwound:171"
      "Children's Playground"
      [Exterior]
      Triangle
      [Circle, Plus, Squiggle]
      Set.AWorldTornDownAgain

sportsField_172 :: CardDef
sportsField_172 =
  location
    ":ages-unwound:172"
    "Sports Field"
    [Exterior]
    Plus
    [Square, Triangle]
    Set.AWorldTornDownAgain

ritualCircle_173 :: CardDef
ritualCircle_173 =
  victory 1
    $ locationWithUnrevealed
      ":ages-unwound:173"
      ("Ritual Circle" <:> "A Present, Fractured")
      [Interior, Ritual]
      Star
      [Hourglass, Heart]
      ("Ritual Circle" <:> "The Present, Fractured")
      [Interior, Ritual]
      Star
      [Hourglass, Heart]
      Set.AWorldTornDownAgain

fulcrumOfPossibility_189 :: CardDef
fulcrumOfPossibility_189 =
  otherSideIs ":ages-unwound:189b"
    $ location
      ":ages-unwound:189"
      "Fulcrum of Possibility"
      [Otherworld, Present]
      Circle
      [Square, Triangle, Plus, Diamond, T, Heart]
      Set.TimeRunsOut

fulcrumOfPossibility_189b :: CardDef
fulcrumOfPossibility_189b =
  otherSideIs ":ages-unwound:189"
    $ location
      ":ages-unwound:189b"
      ("Fulcrum of Possibility" <:> "Heart of Corruption")
      [Otherworld, Paradox, Present]
      Circle
      [Square, Triangle, Plus, Diamond, T, Heart]
      Set.TimeRunsOut

fulcrumOfPossibility_190 :: CardDef
fulcrumOfPossibility_190 =
  otherSideIs ":ages-unwound:190b"
    $ location
      ":ages-unwound:190"
      "Fulcrum of Possibility"
      [Otherworld, Present]
      Circle
      [Square, Triangle, Plus, Diamond, T, Heart]
      Set.TimeRunsOut

daysThatNeverWere_191 :: CardDef
daysThatNeverWere_191 =
  otherSideIs ":ages-unwound:191b"
    $ location
      ":ages-unwound:191"
      "Days That Never Were"
      [Otherworld, Past]
      Square
      [Circle, Triangle, T, Hourglass, Equals]
      Set.TimeRunsOut

daysThatNeverWere_191b :: CardDef
daysThatNeverWere_191b =
  otherSideIs ":ages-unwound:191"
    $ location
      ":ages-unwound:191b"
      ("Days That Never Were" <:> "Everything to Nothing")
      [Otherworld, Paradox, Past]
      Square
      [Circle, Triangle, T, Hourglass, Equals]
      Set.TimeRunsOut

daysThatNeverWere_192 :: CardDef
daysThatNeverWere_192 =
  otherSideIs ":ages-unwound:192b"
    $ location
      ":ages-unwound:192"
      "Days That Never Were"
      [Otherworld, Past]
      Square
      [Circle, Triangle, T, Hourglass, Equals]
      Set.TimeRunsOut

secretsLongForgotten_193 :: CardDef
secretsLongForgotten_193 =
  otherSideIs ":ages-unwound:193b"
    $ location
      ":ages-unwound:193"
      "Secrets Long Forgotten"
      [Otherworld, Past]
      Triangle
      [Circle, Square, Plus, Hourglass, Equals]
      Set.TimeRunsOut

secretsLongForgotten_194 :: CardDef
secretsLongForgotten_194 =
  otherSideIs ":ages-unwound:194b"
    $ location
      ":ages-unwound:194"
      "Secrets Long Forgotten"
      [Otherworld, Past]
      Triangle
      [Circle, Square, Plus, Hourglass, Equals]
      Set.TimeRunsOut

todayAThousandTimes_195 :: CardDef
todayAThousandTimes_195 =
  otherSideIs ":ages-unwound:195b"
    $ location
      ":ages-unwound:195"
      "Today, a Thousand Times"
      [Otherworld, Present]
      Plus
      [Circle, Triangle, Diamond, Heart]
      Set.TimeRunsOut

todayAThousandTimes_196 :: CardDef
todayAThousandTimes_196 =
  otherSideIs ":ages-unwound:196b"
    $ location
      ":ages-unwound:196"
      "Today, a Thousand Times"
      [Otherworld, Present]
      Plus
      [Circle, Triangle, Diamond, Heart]
      Set.TimeRunsOut

whatCouldBe_197 :: CardDef
whatCouldBe_197 =
  otherSideIs ":ages-unwound:197b"
    $ location
      ":ages-unwound:197"
      "What Could Be"
      [Otherworld, Future]
      Diamond
      [Circle, Plus, T, Moon, Star]
      Set.TimeRunsOut

whatCouldBe_198 :: CardDef
whatCouldBe_198 =
  otherSideIs ":ages-unwound:198b"
    $ location
      ":ages-unwound:198"
      "What Could Be"
      [Otherworld, Future]
      Diamond
      [Circle, Plus, T, Moon, Star]
      Set.TimeRunsOut

whatCouldNeverBe_199 :: CardDef
whatCouldNeverBe_199 =
  otherSideIs ":ages-unwound:199b"
    $ location
      ":ages-unwound:199"
      "What Could Never Be"
      [Otherworld, Future]
      T
      [Circle, Square, Diamond, Moon, Star]
      Set.TimeRunsOut

whatCouldNeverBe_200 :: CardDef
whatCouldNeverBe_200 =
  otherSideIs ":ages-unwound:200b"
    $ location
      ":ages-unwound:200"
      "What Could Never Be"
      [Otherworld, Future]
      T
      [Circle, Square, Diamond, Moon, Star]
      Set.TimeRunsOut

dawnOfTheUniverse_201 :: CardDef
dawnOfTheUniverse_201 =
  otherSideIs ":ages-unwound:201b"
    $ location
      ":ages-unwound:201"
      "Dawn of the Universe"
      [Otherworld, Past]
      Hourglass
      [Square, Triangle, Equals]
      Set.TimeRunsOut

dawnOfTheUniverse_201b :: CardDef
dawnOfTheUniverse_201b =
  otherSideIs ":ages-unwound:201"
    $ location
      ":ages-unwound:201b"
      ("Dawn of the Universe" <:> "Perverted Creation")
      [Otherworld, Paradox, Past]
      Hourglass
      [Square, Triangle, Equals]
      Set.TimeRunsOut

dawnOfTheUniverse_202 :: CardDef
dawnOfTheUniverse_202 =
  otherSideIs ":ages-unwound:202b"
    $ location
      ":ages-unwound:202"
      "Dawn of the Universe"
      [Otherworld, Past]
      Hourglass
      [Square, Triangle, Equals]
      Set.TimeRunsOut

dawnOfTheUniverse_202b :: CardDef
dawnOfTheUniverse_202b =
  otherSideIs ":ages-unwound:202"
    $ location
      ":ages-unwound:202b"
      ("Dawn of the Universe" <:> "Time Runs Backwards")
      [Otherworld, Paradox, Past]
      Hourglass
      [Square, Triangle, Equals]
      Set.TimeRunsOut

theEndOfAllThings_203 :: CardDef
theEndOfAllThings_203 =
  otherSideIs ":ages-unwound:203b"
    $ location
      ":ages-unwound:203"
      "The End of All Things"
      [Otherworld, Future]
      Moon
      [Diamond, T, Star]
      Set.TimeRunsOut

theEndOfAllThings_204 :: CardDef
theEndOfAllThings_204 =
  otherSideIs ":ages-unwound:204b"
    $ location
      ":ages-unwound:204"
      "The End of All Things"
      [Otherworld, Future]
      Moon
      [Diamond, T, Star]
      Set.TimeRunsOut

thePast :: CardDef
thePast =
  location
    ":ages-unwound:205"
    "The Past"
    [Otherworld, Past]
    Equals
    [Square, Triangle, Hourglass, Heart]
    Set.TimeRunsOut

thePresent :: CardDef
thePresent =
  location
    ":ages-unwound:206"
    "The Present"
    [Otherworld, Present]
    Heart
    [Circle, Plus, Equals, Star]
    Set.TimeRunsOut

theFuture :: CardDef
theFuture =
  location
    ":ages-unwound:207"
    "The Future"
    [Otherworld, Future]
    Star
    [Diamond, T, Moon, Heart]
    Set.TimeRunsOut

frontHallway :: CardDef
frontHallway =
  victory 1
    $ location
      ":ages-unwound:222"
      "Front Hallway"
      [Interior]
      Squiggle
      [Square, Moon, Equals, Hourglass]
      Set.NightOfTheRitual

rearCorridors :: CardDef
rearCorridors =
  location
    ":ages-unwound:223"
    "Rear Corridors"
    [Interior]
    Diamond
    [Square, T, Heart]
    Set.NightOfTheRitual

ritualCircle_224 :: CardDef
ritualCircle_224 =
  victory 1
    $ location
      ":ages-unwound:224"
      ("Ritual Circle" <:> "A Harnessed Future")
      [Interior, Ritual]
      Hourglass
      [Squiggle, Heart, Star]
      Set.NightOfTheRitual

ritualCircle_225 :: CardDef
ritualCircle_225 =
  victory 1
    $ location
      ":ages-unwound:225"
      ("Ritual Circle" <:> "Gateway to the Past")
      [Interior, Ritual]
      Heart
      [Diamond, Hourglass, Star]
      Set.NightOfTheRitual

cafeteria :: CardDef
cafeteria =
  victory 1
    $ location
      ":ages-unwound:226"
      "Cafeteria"
      [Interior]
      Equals
      [Squiggle]
      Set.NightOfTheRitual

classroom_227 :: CardDef
classroom_227 =
  victory 1
    $ locationWithUnrevealed
      ":ages-unwound:227"
      "Classroom"
      [Interior]
      T
      [Diamond]
      ("Classroom" <:> "Ritual Supplies")
      [Interior]
      T
      [Diamond]
      Set.NightOfTheRitual

classroom_228 :: CardDef
classroom_228 =
  victory 1
    $ locationWithUnrevealed
      ":ages-unwound:228"
      "Classroom"
      [Interior]
      T
      [Diamond]
      ("Classroom" <:> "Guarded Secrets")
      [Interior]
      T
      [Diamond]
      Set.NightOfTheRitual

classroom_229 :: CardDef
classroom_229 =
  locationWithUnrevealed
    ":ages-unwound:229"
    "Classroom"
    [Interior]
    T
    [Diamond]
    ("Classroom" <:> "Energised and Unstable")
    [Interior]
    T
    [Diamond]
    Set.NightOfTheRitual

principalsOffice :: CardDef
principalsOffice =
  locationWithUnrevealed
    ":ages-unwound:230"
    "Principal's Office"
    [Interior]
    Moon
    [Squiggle]
    ("Principal's Office" <:> "Mounting Dread")
    [Interior]
    Moon
    [Squiggle]
    Set.NightOfTheRitual
