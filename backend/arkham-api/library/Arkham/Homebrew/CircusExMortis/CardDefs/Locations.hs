module Arkham.Homebrew.CircusExMortis.CardDefs.Locations where

import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Traits
import Arkham.Location.CardDefs.Import

-- one_night_only
circusGatesPathToFreedom :: CardDef
circusGatesPathToFreedom =
  location
    ":circus-ex-mortis:011"
    ("Circus Gates" <:> "Path to Freedom")
    [NewMoonCircus]
    Moon
    [Trefoil, Spade]
    Set.OneNightOnly

-- the_primrose_path
forestPassage :: CardDef
forestPassage =
  location
    ":circus-ex-mortis:021"
    "Forest Passage"
    [Path]
    Trefoil
    [Moon, Star, Circle, Square, Plus]
    Set.ThePrimrosePath

remoteCabin :: CardDef
remoteCabin =
  victory 1
    $ location ":circus-ex-mortis:022" "Remote Cabin" [Woods] Equals [Moon, Trefoil] Set.ThePrimrosePath

woodlandOverlook :: CardDef
woodlandOverlook =
  victory 1
    $ location
      ":circus-ex-mortis:023"
      "Woodland Overlook"
      [Woods]
      Squiggle
      [Moon, Trefoil]
      Set.ThePrimrosePath

circusEncampment :: CardDef
circusEncampment =
  location
    ":circus-ex-mortis:024"
    "Circus Encampment"
    [Clearing]
    Moon
    [Trefoil, Squiggle, Equals, Circle, Square]
    Set.ThePrimrosePath

moonlitForestSmolderingCampfire :: CardDef
moonlitForestSmolderingCampfire =
  location_
    ":circus-ex-mortis:025"
    ("Moonlit Forest" <:> "Smoldering Campfire")
    [Woods]
    Set.ThePrimrosePath

moonlitForestQuietValley :: CardDef
moonlitForestQuietValley =
  locationWithUnrevealedName
    ":circus-ex-mortis:026"
    "Moonlit Forest"
    ("Moonlit Forest" <:> "Quiet Valley")
    [Woods]
    Star
    [Trefoil]
    Set.ThePrimrosePath

moonlitForestShallowRiver :: CardDef
moonlitForestShallowRiver =
  locationWithUnrevealedName
    ":circus-ex-mortis:027"
    "Moonlit Forest"
    ("Moonlit Forest" <:> "Shallow River")
    [Woods]
    Star
    [Trefoil]
    Set.ThePrimrosePath

moonlitForestGlassyLake :: CardDef
moonlitForestGlassyLake =
  locationWithUnrevealedName
    ":circus-ex-mortis:028"
    "Moonlit Forest"
    ("Moonlit Forest" <:> "Glassy Lake")
    [Woods]
    Star
    [Trefoil]
    Set.ThePrimrosePath

moonlitForestCircularGrove :: CardDef
moonlitForestCircularGrove =
  locationWithUnrevealed
    ":circus-ex-mortis:029"
    "Moonlit Forest"
    [Woods]
    Star
    [Trefoil]
    ("Moonlit Forest" <:> "Circular Grove")
    [Woods]
    Circle
    [Trefoil, Moon]
    Set.ThePrimrosePath

moonlitForestMistyMarsh :: CardDef
moonlitForestMistyMarsh =
  locationWithUnrevealed
    ":circus-ex-mortis:030"
    "Moonlit Forest"
    [Woods]
    Star
    [Trefoil]
    ("Moonlit Forest" <:> "Misty Marsh")
    [Woods]
    Square
    [Trefoil, Moon]
    Set.ThePrimrosePath

moonlitForestShadowedPath :: CardDef
moonlitForestShadowedPath =
  victory 1
    $ locationWithUnrevealed
      ":circus-ex-mortis:031"
      "Moonlit Forest"
      [Woods]
      Star
      [Trefoil]
      ("Moonlit Forest" <:> "Shadowed Path")
      [Woods]
      Plus
      [Trefoil]
      Set.ThePrimrosePath

moonlitForestFogBank :: CardDef
moonlitForestFogBank =
  victory 1
    $ locationWithUnrevealed
      ":circus-ex-mortis:032"
      "Moonlit Forest"
      [Woods]
      Star
      [Trefoil]
      ("Moonlit Forest" <:> "Fog Bank")
      [Woods]
      Plus
      [Trefoil]
      Set.ThePrimrosePath

moonlitForestLabyrinthOfTrees :: CardDef
moonlitForestLabyrinthOfTrees =
  victory 1
    $ locationWithUnrevealed
      ":circus-ex-mortis:033"
      "Moonlit Forest"
      [Woods]
      Star
      [Trefoil]
      ("Moonlit Forest" <:> "Labyrinth of Trees")
      [Woods]
      Plus
      [Trefoil]
      Set.ThePrimrosePath

moonlitForestDeadGrove :: CardDef
moonlitForestDeadGrove =
  victory 1
    $ locationWithUnrevealed
      ":circus-ex-mortis:034"
      "Moonlit Forest"
      [Woods]
      Star
      [Trefoil]
      ("Moonlit Forest" <:> "Dead Grove")
      [Woods]
      Plus
      [Trefoil]
      Set.ThePrimrosePath

-- harm_s_way
ringmastersTrailer :: CardDef
ringmastersTrailer =
  location ":circus-ex-mortis:047" "Ringmaster's Trailer" [Camp] Moon [Circle, Square] Set.HarmsWay

crowdedRow_048 :: CardDef
crowdedRow_048 =
  location ":circus-ex-mortis:048" "Crowded Row" [Camp] Circle [Moon, Square] Set.HarmsWay

crowdedRow_049 :: CardDef
crowdedRow_049 =
  location ":circus-ex-mortis:049" "Crowded Row" [Camp] Circle [Moon, Square] Set.HarmsWay

crowdedRow_050 :: CardDef
crowdedRow_050 =
  location ":circus-ex-mortis:050" "Crowded Row" [Camp] Circle [Moon, Square] Set.HarmsWay

crowdedRow_051 :: CardDef
crowdedRow_051 =
  location ":circus-ex-mortis:051" "Crowded Row" [Camp] Circle [Moon, Square] Set.HarmsWay

secludedTent_052 :: CardDef
secludedTent_052 =
  location ":circus-ex-mortis:052" "Secluded Tent" [Camp] Square [Moon, Circle] Set.HarmsWay

secludedTent_053 :: CardDef
secludedTent_053 =
  location ":circus-ex-mortis:053" "Secluded Tent" [Camp] Square [Moon, Circle] Set.HarmsWay

secludedTent_054 :: CardDef
secludedTent_054 =
  location ":circus-ex-mortis:054" "Secluded Tent" [Camp] Square [Moon, Circle] Set.HarmsWay

secludedTent_055 :: CardDef
secludedTent_055 =
  location ":circus-ex-mortis:055" "Secluded Tent" [Camp] Square [Moon, Circle] Set.HarmsWay

campOutskirtsGuardedClosely :: CardDef
campOutskirtsGuardedClosely =
  location
    ":circus-ex-mortis:056"
    ("Camp Outskirts" <:> "Guarded Closely")
    [Woods]
    Star
    []
    Set.HarmsWay

campOutskirtsQuietForNow :: CardDef
campOutskirtsQuietForNow =
  location
    ":circus-ex-mortis:057"
    ("Camp Outskirts" <:> "Quiet, For Now")
    [Woods]
    Star
    []
    Set.HarmsWay

-- all_points_west
caboose :: CardDef
caboose =
  singleSided $ location ":circus-ex-mortis:081" "Caboose" [Train] Heart [Star] Set.AllPointsWest

locomotiveEngine :: CardDef
locomotiveEngine =
  singleSided
    $ location ":circus-ex-mortis:082" "Locomotive Engine" [Train] Equals [Square, Moon] Set.AllPointsWest

boxcar :: CardDef
boxcar =
  singleSided
    $ location
      ":circus-ex-mortis:083"
      "Boxcar"
      [Train, FreightCar]
      Square
      [Equals, Star, Hourglass]
      Set.AllPointsWest

flatcar :: CardDef
flatcar =
  singleSided
    $ location
      ":circus-ex-mortis:084"
      "Flatcar"
      [Train, FreightCar]
      Square
      [Equals, Star, Hourglass]
      Set.AllPointsWest

gondolaCar :: CardDef
gondolaCar =
  singleSided
    $ location
      ":circus-ex-mortis:085"
      "Gondola Car"
      [Train, FreightCar]
      Square
      [Equals, Star, Hourglass]
      Set.AllPointsWest

stockCar :: CardDef
stockCar =
  singleSided
    $ location
      ":circus-ex-mortis:086"
      "Stock Car"
      [Train, FreightCar]
      Square
      [Equals, Star, Hourglass]
      Set.AllPointsWest

tankCar :: CardDef
tankCar =
  singleSided
    $ location
      ":circus-ex-mortis:087"
      "Tank Car"
      [Train, FreightCar]
      Square
      [Equals, Star, Hourglass]
      Set.AllPointsWest

coalHopperCar :: CardDef
coalHopperCar =
  singleSided
    $ victory 1
    $ location
      ":circus-ex-mortis:088"
      "Coal Hopper Car"
      [Train, SpecialCar]
      Star
      [Square, Heart, Plus]
      Set.AllPointsWest

craneCar :: CardDef
craneCar =
  singleSided
    $ victory 1
    $ location
      ":circus-ex-mortis:089"
      "Crane Car"
      [Train, SpecialCar]
      Star
      [Square, Heart, Plus]
      Set.AllPointsWest

mailCar :: CardDef
mailCar =
  singleSided
    $ victory 1
    $ location
      ":circus-ex-mortis:090"
      "Mail Car"
      [Train, SpecialCar]
      Star
      [Square, Heart, Plus]
      Set.AllPointsWest

refrigeratorCar :: CardDef
refrigeratorCar =
  singleSided
    $ victory 1
    $ location
      ":circus-ex-mortis:091"
      "Refrigerator Car"
      [Train, SpecialCar]
      Star
      [Square, Heart, Plus]
      Set.AllPointsWest

reinforcedCar :: CardDef
reinforcedCar =
  singleSided
    $ victory 1
    $ location
      ":circus-ex-mortis:092"
      "Reinforced Car"
      [Train, SpecialCar]
      Star
      [Square, Heart, Plus]
      Set.AllPointsWest

circusEngine :: CardDef
circusEngine =
  location
    ":circus-ex-mortis:093"
    "Circus Engine"
    [CircusTrain]
    Moon
    [Hourglass, Equals]
    Set.AllPointsWest

exoticAnimalCar :: CardDef
exoticAnimalCar =
  victory 1
    $ location
      ":circus-ex-mortis:094"
      "Exotic Animal Car"
      [CircusTrain]
      Plus
      [Hourglass, Star]
      Set.AllPointsWest

performersCar :: CardDef
performersCar =
  location
    ":circus-ex-mortis:095"
    "Performers' Car"
    [CircusTrain]
    Hourglass
    [Moon, Plus, Square]
    Set.AllPointsWest

-- piper_at_the_gates_of_dawn
circusGatesDoorwayToDoom :: CardDef
circusGatesDoorwayToDoom =
  location
    ":circus-ex-mortis:117"
    ("Circus Gates" <:> "Doorway to Doom")
    [NewMoonCircus]
    Moon
    [Trefoil, Spade]
    Set.PiperAtTheGatesOfDawn

-- bacchanalia
vestibule :: CardDef
vestibule =
  location
    ":circus-ex-mortis:129"
    "Vestibule"
    [LiberPater]
    Hourglass
    [Circle, Heart, Triangle, Trefoil]
    Set.Bacchanalia

banquetHall :: CardDef
banquetHall =
  location
    ":circus-ex-mortis:130"
    "Banquet Hall"
    [LiberPater]
    Circle
    [Hourglass, Heart, Squiggle]
    Set.Bacchanalia

statuaryGardens :: CardDef
statuaryGardens =
  location
    ":circus-ex-mortis:131"
    "Statuary Gardens"
    [LiberPater]
    Trefoil
    [Hourglass, Triangle, Equals]
    Set.Bacchanalia

privateParlor :: CardDef
privateParlor =
  location
    ":circus-ex-mortis:132"
    "Private Parlor"
    [LiberPater]
    Heart
    [Hourglass, Circle, Triangle, Star]
    Set.Bacchanalia

collectionHall :: CardDef
collectionHall =
  location
    ":circus-ex-mortis:133"
    "Collection Hall"
    [LiberPater]
    Triangle
    [Hourglass, Trefoil, Heart, Star]
    Set.Bacchanalia

upperBalcony :: CardDef
upperBalcony =
  location ":circus-ex-mortis:134" "Upper Balcony" [LiberPater] Star [Heart, Triangle] Set.Bacchanalia

hiddenDungeon :: CardDef
hiddenDungeon =
  location
    ":circus-ex-mortis:135"
    "Hidden Dungeon"
    [LiberPater, Restricted]
    Equals
    [Trefoil, Moon]
    Set.Bacchanalia

manorCellars :: CardDef
manorCellars =
  location
    ":circus-ex-mortis:136"
    "Manor Cellars"
    [LiberPater, Restricted]
    Squiggle
    [Circle, Moon]
    Set.Bacchanalia

savageAltar :: CardDef
savageAltar =
  victory 2
    $ location
      ":circus-ex-mortis:137"
      "Savage Altar"
      [LiberPater, Restricted]
      Moon
      [Squiggle, Equals]
      Set.Bacchanalia

-- red_sunrise
forgottenTrail :: CardDef
forgottenTrail =
  location ":circus-ex-mortis:160" "Forgotten Trail" [Woods] Trefoil [T] Set.RedSunrise

ritualClearing :: CardDef
ritualClearing =
  victory 1 $ location ":circus-ex-mortis:161" "Ritual Clearing" [Woods] Moon [Heart] Set.RedSunrise

foothillSlope_162 :: CardDef
foothillSlope_162 =
  location ":circus-ex-mortis:162" "Foothill Slope" [Woods] Star [T, Star] Set.RedSunrise

foothillSlope_163 :: CardDef
foothillSlope_163 =
  location ":circus-ex-mortis:163" "Foothill Slope" [Woods] Star [T, Star] Set.RedSunrise

foothillSlope_164 :: CardDef
foothillSlope_164 =
  location ":circus-ex-mortis:164" "Foothill Slope" [Woods] Star [T, Star] Set.RedSunrise

foothillSlope_165 :: CardDef
foothillSlope_165 =
  location ":circus-ex-mortis:165" "Foothill Slope" [Woods] Star [T, Star] Set.RedSunrise

mountainStream_166 :: CardDef
mountainStream_166 =
  location ":circus-ex-mortis:166" "Mountain Stream" [Woods] Diamond [Star, Diamond] Set.RedSunrise

mountainStream_167 :: CardDef
mountainStream_167 =
  location ":circus-ex-mortis:167" "Mountain Stream" [Woods] Diamond [Star, Diamond] Set.RedSunrise

mountainStream_168 :: CardDef
mountainStream_168 =
  location ":circus-ex-mortis:168" "Mountain Stream" [Woods] Diamond [Star, Diamond] Set.RedSunrise

mountainStream_169 :: CardDef
mountainStream_169 =
  location ":circus-ex-mortis:169" "Mountain Stream" [Woods] Diamond [Star, Diamond] Set.RedSunrise

openForest_170 :: CardDef
openForest_170 =
  location ":circus-ex-mortis:170" "Open Forest" [Woods] T [Trefoil, T] Set.RedSunrise

openForest_171 :: CardDef
openForest_171 =
  location ":circus-ex-mortis:171" "Open Forest" [Woods] T [Trefoil, T] Set.RedSunrise

openForest_172 :: CardDef
openForest_172 =
  location ":circus-ex-mortis:172" "Open Forest" [Woods] T [Trefoil, T] Set.RedSunrise

shadowedWilderness_173 :: CardDef
shadowedWilderness_173 =
  location ":circus-ex-mortis:173" "Shadowed Wilderness" [Woods] Heart [Diamond, Heart] Set.RedSunrise

shadowedWilderness_174 :: CardDef
shadowedWilderness_174 =
  location ":circus-ex-mortis:174" "Shadowed Wilderness" [Woods] Heart [Diamond, Heart] Set.RedSunrise

shadowedWilderness_175 :: CardDef
shadowedWilderness_175 =
  location ":circus-ex-mortis:175" "Shadowed Wilderness" [Woods] Heart [Diamond, Heart] Set.RedSunrise

shadowedWilderness_176 :: CardDef
shadowedWilderness_176 =
  location ":circus-ex-mortis:176" "Shadowed Wilderness" [Woods] Heart [Diamond, Heart] Set.RedSunrise

shadowedWilderness_177 :: CardDef
shadowedWilderness_177 =
  location ":circus-ex-mortis:177" "Shadowed Wilderness" [Woods] Heart [Diamond, Heart] Set.RedSunrise

-- thousand_to_one
silentClearing :: CardDef
silentClearing =
  location
    ":circus-ex-mortis:195"
    "Silent Clearing"
    [Woods]
    Moon
    [T, Trefoil, Droplet, Star, Plus]
    Set.ThousandToOne

primalForest :: CardDef
primalForest =
  victory 1
    $ location ":circus-ex-mortis:196" "Primal Forest" [Woods, Tainted] Heart [Triangle] Set.ThousandToOne

highThicket :: CardDef
highThicket =
  location
    ":circus-ex-mortis:197"
    "High Thicket"
    [Woods, Tainted]
    Triangle
    [Heart, T]
    Set.ThousandToOne

sparseWoodland :: CardDef
sparseWoodland =
  location
    ":circus-ex-mortis:198"
    "Sparse Woodland"
    [Woods, Tainted]
    T
    [Triangle, Moon]
    Set.ThousandToOne

mossyGlen :: CardDef
mossyGlen =
  location ":circus-ex-mortis:199" "Mossy Glen" [Woods] Trefoil [Droplet, Moon] Set.ThousandToOne

fallenCopse :: CardDef
fallenCopse =
  location ":circus-ex-mortis:200" "Fallen Copse" [Woods] Droplet [Trefoil, Moon] Set.ThousandToOne

forestChasm :: CardDef
forestChasm =
  otherSideIs ":circus-ex-mortis:203"
    $ location ":circus-ex-mortis:203b" "Forest Chasm" [Woods] Star [Plus, Moon] Set.ThousandToOne

canyonEntrance :: CardDef
canyonEntrance =
  otherSideIs ":circus-ex-mortis:204"
    $ location ":circus-ex-mortis:204b" "Canyon Entrance" [Woods] Plus [Star, Moon] Set.ThousandToOne

markedGrove :: CardDef
markedGrove =
  otherSideIs ":circus-ex-mortis:205"
    $ location ":circus-ex-mortis:205b" "Marked Grove" [Woods] Star [Plus, Moon] Set.ThousandToOne

defiledWoods :: CardDef
defiledWoods =
  otherSideIs ":circus-ex-mortis:206"
    $ location ":circus-ex-mortis:206b" "Defiled Woods" [Woods] Plus [Star, Moon] Set.ThousandToOne

-- circus_grounds
animalCages :: CardDef
animalCages =
  victory 1
    $ location
      ":circus-ex-mortis:219"
      "Animal Cages"
      [NewMoonCircus]
      Hourglass
      [Squiggle, Trefoil]
      Set.CircusGrounds

carousel :: CardDef
carousel =
  location
    ":circus-ex-mortis:220"
    "Carousel"
    [NewMoonCircus]
    Trefoil
    [Triangle, Hourglass, Spade, Moon]
    Set.CircusGrounds

gamesGallery :: CardDef
gamesGallery =
  location
    ":circus-ex-mortis:221"
    "Games Gallery"
    [NewMoonCircus]
    Spade
    [Triangle, Heart, Trefoil, Moon]
    Set.CircusGrounds

performerTrailers :: CardDef
performerTrailers =
  victory 1
    $ location
      ":circus-ex-mortis:222"
      "Performer Trailers"
      [NewMoonCircus]
      Heart
      [Equals, Spade]
      Set.CircusGrounds

theBigTopFirstRing :: CardDef
theBigTopFirstRing =
  location
    ":circus-ex-mortis:223"
    ("The Big Top" <:> "First Ring")
    [NewMoonCircus]
    Triangle
    [Squiggle, Equals, Trefoil, Spade]
    Set.CircusGrounds

theBigTopSecondRing :: CardDef
theBigTopSecondRing =
  location
    ":circus-ex-mortis:224"
    ("The Big Top" <:> "Second Ring")
    [NewMoonCircus]
    Squiggle
    [Triangle, Equals, Hourglass]
    Set.CircusGrounds

theBigTopThirdRing :: CardDef
theBigTopThirdRing =
  location
    ":circus-ex-mortis:225"
    ("The Big Top" <:> "Third Ring")
    [NewMoonCircus]
    Equals
    [Triangle, Squiggle, Heart]
    Set.CircusGrounds
