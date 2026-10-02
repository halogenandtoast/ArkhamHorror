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
    $ locationWithUnrevealed
      ":circus-ex-mortis:022"
      "Remote Cabin"
      [Woods]
      Equals
      []
      "Remote Cabin"
      [Woods]
      Equals
      [Moon, Trefoil]
      Set.ThePrimrosePath

woodlandOverlook :: CardDef
woodlandOverlook =
  victory 1
    $ locationWithUnrevealed
      ":circus-ex-mortis:023"
      "Woodland Overlook"
      [Woods]
      Squiggle
      []
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

{- | The Primrose Path's ten copies of Moonlit Forest all hide behind the same
unrevealed face, and differ only in the subtitle, symbol and connections they reveal.
-}
moonlitForest :: CardCode -> Text -> LocationSymbol -> [LocationSymbol] -> CardDef
moonlitForest code subtitle symbol connections =
  locationWithUnrevealed
    code
    "Moonlit Forest"
    [Woods]
    Star
    [Trefoil]
    ("Moonlit Forest" <:> subtitle)
    [Woods]
    symbol
    connections
    Set.ThePrimrosePath

moonlitForestSmolderingCampfire :: CardDef
moonlitForestSmolderingCampfire =
  moonlitForest ":circus-ex-mortis:025" "Smoldering Campfire" Star [Trefoil]

moonlitForestQuietValley :: CardDef
moonlitForestQuietValley =
  moonlitForest ":circus-ex-mortis:026" "Quiet Valley" Star [Trefoil]

moonlitForestShallowRiver :: CardDef
moonlitForestShallowRiver =
  moonlitForest ":circus-ex-mortis:027" "Shallow River" Star [Trefoil]

moonlitForestGlassyLake :: CardDef
moonlitForestGlassyLake =
  moonlitForest ":circus-ex-mortis:028" "Glassy Lake" Star [Trefoil]

moonlitForestCircularGrove :: CardDef
moonlitForestCircularGrove =
  moonlitForest ":circus-ex-mortis:029" "Circular Grove" Circle [Trefoil, Moon]

moonlitForestMistyMarsh :: CardDef
moonlitForestMistyMarsh =
  moonlitForest ":circus-ex-mortis:030" "Misty Marsh" Square [Trefoil, Moon]

moonlitForestShadowedPath :: CardDef
moonlitForestShadowedPath =
  victory 1 $ moonlitForest ":circus-ex-mortis:031" "Shadowed Path" Plus [Trefoil]

moonlitForestFogBank :: CardDef
moonlitForestFogBank =
  victory 1 $ moonlitForest ":circus-ex-mortis:032" "Fog Bank" Plus [Trefoil]

moonlitForestLabyrinthOfTrees :: CardDef
moonlitForestLabyrinthOfTrees =
  victory 1 $ moonlitForest ":circus-ex-mortis:033" "Labyrinth of Trees" Plus [Trefoil]

moonlitForestDeadGrove :: CardDef
moonlitForestDeadGrove =
  victory 1 $ moonlitForest ":circus-ex-mortis:034" "Dead Grove" Plus [Trefoil]

-- harm_s_way
ringmastersTrailer :: CardDef
ringmastersTrailer =
  location ":circus-ex-mortis:047" "Ringmaster's Trailer" [Camp] Moon [Circle, Square] Set.HarmsWay

-- | The four Crowded Row copies share one printed face; each prints its own ability.
crowdedRow :: CardCode -> CardDef
crowdedRow code = location code "Crowded Row" [Camp] Circle [Moon, Square] Set.HarmsWay

crowdedRow_048 :: CardDef
crowdedRow_048 = crowdedRow ":circus-ex-mortis:048"

crowdedRow_049 :: CardDef
crowdedRow_049 = crowdedRow ":circus-ex-mortis:049"

crowdedRow_050 :: CardDef
crowdedRow_050 = crowdedRow ":circus-ex-mortis:050"

crowdedRow_051 :: CardDef
crowdedRow_051 = crowdedRow ":circus-ex-mortis:051"

-- | As with 'crowdedRow', the four Secluded Tent copies differ only in their abilities.
secludedTent :: CardCode -> CardDef
secludedTent code = location code "Secluded Tent" [Camp] Square [Moon, Circle] Set.HarmsWay

secludedTent_052 :: CardDef
secludedTent_052 = secludedTent ":circus-ex-mortis:052"

secludedTent_053 :: CardDef
secludedTent_053 = secludedTent ":circus-ex-mortis:053"

secludedTent_054 :: CardDef
secludedTent_054 = secludedTent ":circus-ex-mortis:054"

secludedTent_055 :: CardDef
secludedTent_055 = secludedTent ":circus-ex-mortis:055"

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

{- | The train's five Freight Cars are interchangeable but for their names, and so are
its five Special Cars, which are each worth 1 victory point.
-}
freightCar :: CardCode -> Name -> CardDef
freightCar code name =
  singleSided
    $ location code name [Train, FreightCar] Square [Equals, Star, Hourglass] Set.AllPointsWest

specialCar :: CardCode -> Name -> CardDef
specialCar code name =
  singleSided
    $ victory 1
    $ location code name [Train, SpecialCar] Star [Square, Heart, Plus] Set.AllPointsWest

boxcar :: CardDef
boxcar = freightCar ":circus-ex-mortis:083" "Boxcar"

flatcar :: CardDef
flatcar = freightCar ":circus-ex-mortis:084" "Flatcar"

gondolaCar :: CardDef
gondolaCar = freightCar ":circus-ex-mortis:085" "Gondola Car"

stockCar :: CardDef
stockCar = freightCar ":circus-ex-mortis:086" "Stock Car"

tankCar :: CardDef
tankCar = freightCar ":circus-ex-mortis:087" "Tank Car"

coalHopperCar :: CardDef
coalHopperCar = specialCar ":circus-ex-mortis:088" "Coal Hopper Car"

craneCar :: CardDef
craneCar = specialCar ":circus-ex-mortis:089" "Crane Car"

mailCar :: CardDef
mailCar = specialCar ":circus-ex-mortis:090" "Mail Car"

refrigeratorCar :: CardDef
refrigeratorCar = specialCar ":circus-ex-mortis:091" "Refrigerator Car"

reinforcedCar :: CardDef
reinforcedCar = specialCar ":circus-ex-mortis:092" "Reinforced Car"

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

{- | Red Sunrise builds its rows from four interchangeable copies of each location; only
the abilities differ between copies.
-}
foothillSlope :: CardCode -> CardDef
foothillSlope code = location code "Foothill Slope" [Woods] Star [T, Star] Set.RedSunrise

foothillSlope_162 :: CardDef
foothillSlope_162 = foothillSlope ":circus-ex-mortis:162"

foothillSlope_163 :: CardDef
foothillSlope_163 = foothillSlope ":circus-ex-mortis:163"

foothillSlope_164 :: CardDef
foothillSlope_164 = foothillSlope ":circus-ex-mortis:164"

foothillSlope_165 :: CardDef
foothillSlope_165 = foothillSlope ":circus-ex-mortis:165"

-- | See 'foothillSlope'.
mountainStream :: CardCode -> CardDef
mountainStream code = location code "Mountain Stream" [Woods] Diamond [Star, Diamond] Set.RedSunrise

mountainStream_166 :: CardDef
mountainStream_166 = mountainStream ":circus-ex-mortis:166"

mountainStream_167 :: CardDef
mountainStream_167 = mountainStream ":circus-ex-mortis:167"

mountainStream_168 :: CardDef
mountainStream_168 = mountainStream ":circus-ex-mortis:168"

mountainStream_169 :: CardDef
mountainStream_169 = mountainStream ":circus-ex-mortis:169"

-- | See 'foothillSlope'.
openForest :: CardCode -> CardDef
openForest code = location code "Open Forest" [Woods] T [Trefoil, T] Set.RedSunrise

openForest_170 :: CardDef
openForest_170 = openForest ":circus-ex-mortis:170"

openForest_171 :: CardDef
openForest_171 = openForest ":circus-ex-mortis:171"

openForest_172 :: CardDef
openForest_172 = openForest ":circus-ex-mortis:172"

-- | See 'foothillSlope'.
shadowedWilderness :: CardCode -> CardDef
shadowedWilderness code = location code "Shadowed Wilderness" [Woods] Heart [Diamond, Heart] Set.RedSunrise

shadowedWilderness_173 :: CardDef
shadowedWilderness_173 = shadowedWilderness ":circus-ex-mortis:173"

shadowedWilderness_174 :: CardDef
shadowedWilderness_174 = shadowedWilderness ":circus-ex-mortis:174"

shadowedWilderness_175 :: CardDef
shadowedWilderness_175 = shadowedWilderness ":circus-ex-mortis:175"

shadowedWilderness_176 :: CardDef
shadowedWilderness_176 = shadowedWilderness ":circus-ex-mortis:176"

shadowedWilderness_177 :: CardDef
shadowedWilderness_177 = shadowedWilderness ":circus-ex-mortis:177"

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
