module Arkham.Homebrew.CircusExMortis.CardDefs.Locations where

import Arkham.Location.CardDefs.Import
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set

-- one_night_only
circusGatesPathToFreedom :: CardDef
circusGatesPathToFreedom =
  location_ "z-circus-ex-mortis-011" ("Circus Gates" <:> "Path to Freedom") [NewMoonCircus] Set.OneNightOnly

-- the_primrose_path
forestPassage :: CardDef
forestPassage =
  location_ "z-circus-ex-mortis-022" "Forest Passage" [Path] Set.ThePrimrosePath

remoteCabin :: CardDef
remoteCabin =
  victory 1 $ location_ "z-circus-ex-mortis-023" "Remote Cabin" [Woods] Set.ThePrimrosePath

woodlandOverlook :: CardDef
woodlandOverlook =
  victory 1 $ location_ "z-circus-ex-mortis-024" "Woodland Overlook" [Woods] Set.ThePrimrosePath

circusEncampment :: CardDef
circusEncampment =
  location_ "z-circus-ex-mortis-025" "Circus Encampment" [Clearing] Set.ThePrimrosePath

moonlitForestSmolderingCampfire :: CardDef
moonlitForestSmolderingCampfire =
  location_ "z-circus-ex-mortis-026" ("Moonlit Forest" <:> "Smoldering Campfire") [Woods] Set.ThePrimrosePath

moonlitForestQuietValley :: CardDef
moonlitForestQuietValley =
  location_ "z-circus-ex-mortis-027" ("Moonlit Forest" <:> "Quiet Valley") [Woods] Set.ThePrimrosePath

moonlitForestShallowRiver :: CardDef
moonlitForestShallowRiver =
  location_ "z-circus-ex-mortis-028" ("Moonlit Forest" <:> "Shallow River") [Woods] Set.ThePrimrosePath

moonlitForestGlassyLake :: CardDef
moonlitForestGlassyLake =
  location_ "z-circus-ex-mortis-029" ("Moonlit Forest" <:> "Glassy Lake") [Woods] Set.ThePrimrosePath

moonlitForestCircularGrove :: CardDef
moonlitForestCircularGrove =
  location_ "z-circus-ex-mortis-031" ("Moonlit Forest" <:> "Circular Grove") [Woods] Set.ThePrimrosePath

moonlitForestMistyMarsh :: CardDef
moonlitForestMistyMarsh =
  location_ "z-circus-ex-mortis-032" ("Moonlit Forest" <:> "Misty Marsh") [Woods] Set.ThePrimrosePath

moonlitForestShadowedPath :: CardDef
moonlitForestShadowedPath =
  victory 1 $ location_ "z-circus-ex-mortis-033" ("Moonlit Forest" <:> "Shadowed Path") [Woods] Set.ThePrimrosePath

moonlitForestFogBank :: CardDef
moonlitForestFogBank =
  victory 1 $ location_ "z-circus-ex-mortis-034" ("Moonlit Forest" <:> "Fog Bank") [Woods] Set.ThePrimrosePath

moonlitForestLabyrinthOfTrees :: CardDef
moonlitForestLabyrinthOfTrees =
  victory 1 $ location_ "z-circus-ex-mortis-035" ("Moonlit Forest" <:> "Labyrinth of Trees") [Woods] Set.ThePrimrosePath

moonlitForestDeadGrove :: CardDef
moonlitForestDeadGrove =
  victory 1 $ location_ "z-circus-ex-mortis-036" ("Moonlit Forest" <:> "Dead Grove") [Woods] Set.ThePrimrosePath

-- harm_s_way
ringmastersTrailer :: CardDef
ringmastersTrailer =
  location_ "z-circus-ex-mortis-049" "Ringmaster's Trailer" [Camp] Set.HarmsWay

crowdedRow_050 :: CardDef
crowdedRow_050 =
  location_ "z-circus-ex-mortis-050" "Crowded Row" [Camp] Set.HarmsWay

crowdedRow_051 :: CardDef
crowdedRow_051 =
  location_ "z-circus-ex-mortis-051" "Crowded Row" [Camp] Set.HarmsWay

crowdedRow_052 :: CardDef
crowdedRow_052 =
  location_ "z-circus-ex-mortis-052" "Crowded Row" [Camp] Set.HarmsWay

crowdedRow_053 :: CardDef
crowdedRow_053 =
  location_ "z-circus-ex-mortis-053" "Crowded Row" [Camp] Set.HarmsWay

secludedTent_054 :: CardDef
secludedTent_054 =
  location_ "z-circus-ex-mortis-054" "Secluded Tent" [Camp] Set.HarmsWay

secludedTent_055 :: CardDef
secludedTent_055 =
  location_ "z-circus-ex-mortis-055" "Secluded Tent" [Camp] Set.HarmsWay

secludedTent_056 :: CardDef
secludedTent_056 =
  location_ "z-circus-ex-mortis-056" "Secluded Tent" [Camp] Set.HarmsWay

secludedTent_057 :: CardDef
secludedTent_057 =
  location_ "z-circus-ex-mortis-057" "Secluded Tent" [Camp] Set.HarmsWay

campOutskirtsGuardedClosely :: CardDef
campOutskirtsGuardedClosely =
  location_ "z-circus-ex-mortis-058" ("Camp Outskirts" <:> "Guarded Closely") [Woods] Set.HarmsWay

campOutskirtsQuietForNow :: CardDef
campOutskirtsQuietForNow =
  location_ "z-circus-ex-mortis-059" ("Camp Outskirts" <:> "Quiet, For Now") [Woods] Set.HarmsWay

-- all_points_west
caboose :: CardDef
caboose =
  location_ "z-circus-ex-mortis-083" "Caboose" [Train] Set.AllPointsWest

locomotiveEngine :: CardDef
locomotiveEngine =
  location_ "z-circus-ex-mortis-084" "Locomotive Engine" [Train] Set.AllPointsWest

boxcar :: CardDef
boxcar =
  location_ "z-circus-ex-mortis-085" "Boxcar" [Train, FreightCar] Set.AllPointsWest

flatcar :: CardDef
flatcar =
  location_ "z-circus-ex-mortis-086" "Flatcar" [Train, FreightCar] Set.AllPointsWest

gondolaCar :: CardDef
gondolaCar =
  location_ "z-circus-ex-mortis-087" "Gondola Car" [Train, FreightCar] Set.AllPointsWest

stockCar :: CardDef
stockCar =
  location_ "z-circus-ex-mortis-088" "Stock Car" [Train, FreightCar] Set.AllPointsWest

tankCar :: CardDef
tankCar =
  location_ "z-circus-ex-mortis-089" "Tank Car" [Train, FreightCar] Set.AllPointsWest

coalHopperCar :: CardDef
coalHopperCar =
  victory 1 $ location_ "z-circus-ex-mortis-090" "Coal Hopper Car" [Train, SpecialCar] Set.AllPointsWest

craneCar :: CardDef
craneCar =
  victory 1 $ location_ "z-circus-ex-mortis-091" "Crane Car" [Train, SpecialCar] Set.AllPointsWest

mailCar :: CardDef
mailCar =
  victory 1 $ location_ "z-circus-ex-mortis-092" "Mail Car" [Train, SpecialCar] Set.AllPointsWest

refrigeratorCar :: CardDef
refrigeratorCar =
  victory 1 $ location_ "z-circus-ex-mortis-093" "Refrigerator Car" [Train, SpecialCar] Set.AllPointsWest

reinforcedCar :: CardDef
reinforcedCar =
  victory 1 $ location_ "z-circus-ex-mortis-094" "Reinforced Car" [Train, SpecialCar] Set.AllPointsWest

circusEngine :: CardDef
circusEngine =
  location_ "z-circus-ex-mortis-095" "Circus Engine" [CircusTrain] Set.AllPointsWest

exoticAnimalCar :: CardDef
exoticAnimalCar =
  victory 1 $ location_ "z-circus-ex-mortis-096" "Exotic Animal Car" [CircusTrain] Set.AllPointsWest

performersCar :: CardDef
performersCar =
  location_ "z-circus-ex-mortis-097" "Performers' Car" [CircusTrain] Set.AllPointsWest

-- piper_at_the_gates_of_dawn
circusGatesDoorwayToDoom :: CardDef
circusGatesDoorwayToDoom =
  location_ "z-circus-ex-mortis-119" ("Circus Gates" <:> "Doorway to Doom") [NewMoonCircus] Set.PiperAtTheGatesOfDawn

-- bacchanalia
vestibule :: CardDef
vestibule =
  location_ "z-circus-ex-mortis-131" "Vestibule" [LiberPater] Set.Bacchanalia

banquetHall :: CardDef
banquetHall =
  location_ "z-circus-ex-mortis-132" "Banquet Hall" [LiberPater] Set.Bacchanalia

statuaryGardens :: CardDef
statuaryGardens =
  location_ "z-circus-ex-mortis-133" "Statuary Gardens" [LiberPater] Set.Bacchanalia

privateParlor :: CardDef
privateParlor =
  location_ "z-circus-ex-mortis-134" "Private Parlor" [LiberPater] Set.Bacchanalia

collectionHall :: CardDef
collectionHall =
  location_ "z-circus-ex-mortis-135" "Collection Hall" [LiberPater] Set.Bacchanalia

upperBalcony :: CardDef
upperBalcony =
  location_ "z-circus-ex-mortis-136" "Upper Balcony" [LiberPater] Set.Bacchanalia

hiddenDungeon :: CardDef
hiddenDungeon =
  location_ "z-circus-ex-mortis-137" "Hidden Dungeon" [LiberPater, Restricted] Set.Bacchanalia

manorCellars :: CardDef
manorCellars =
  location_ "z-circus-ex-mortis-138" "Manor Cellars" [LiberPater, Restricted] Set.Bacchanalia

savageAltar :: CardDef
savageAltar =
  victory 2 $ location_ "z-circus-ex-mortis-139" "Savage Altar" [LiberPater, Restricted] Set.Bacchanalia

-- red_sunrise
forgottenTrail :: CardDef
forgottenTrail =
  location_ "z-circus-ex-mortis-162" "Forgotten Trail" [Woods] Set.RedSunrise

ritualClearing :: CardDef
ritualClearing =
  victory 1 $ location_ "z-circus-ex-mortis-163" "Ritual Clearing" [Woods] Set.RedSunrise

foothillSlope_164 :: CardDef
foothillSlope_164 =
  location_ "z-circus-ex-mortis-164" "Foothill Slope" [Woods] Set.RedSunrise

foothillSlope_165 :: CardDef
foothillSlope_165 =
  location_ "z-circus-ex-mortis-165" "Foothill Slope" [Woods] Set.RedSunrise

foothillSlope_166 :: CardDef
foothillSlope_166 =
  location_ "z-circus-ex-mortis-166" "Foothill Slope" [Woods] Set.RedSunrise

foothillSlope_167 :: CardDef
foothillSlope_167 =
  location_ "z-circus-ex-mortis-167" "Foothill Slope" [Woods] Set.RedSunrise

mountainStream_168 :: CardDef
mountainStream_168 =
  location_ "z-circus-ex-mortis-168" "Mountain Stream" [Woods] Set.RedSunrise

mountainStream_169 :: CardDef
mountainStream_169 =
  location_ "z-circus-ex-mortis-169" "Mountain Stream" [Woods] Set.RedSunrise

mountainStream_170 :: CardDef
mountainStream_170 =
  location_ "z-circus-ex-mortis-170" "Mountain Stream" [Woods] Set.RedSunrise

mountainStream_171 :: CardDef
mountainStream_171 =
  location_ "z-circus-ex-mortis-171" "Mountain Stream" [Woods] Set.RedSunrise

openForest_172 :: CardDef
openForest_172 =
  location_ "z-circus-ex-mortis-172" "Open Forest" [Woods] Set.RedSunrise

openForest_173 :: CardDef
openForest_173 =
  location_ "z-circus-ex-mortis-173" "Open Forest" [Woods] Set.RedSunrise

openForest_174 :: CardDef
openForest_174 =
  location_ "z-circus-ex-mortis-174" "Open Forest" [Woods] Set.RedSunrise

shadowedWilderness_175 :: CardDef
shadowedWilderness_175 =
  location_ "z-circus-ex-mortis-175" "Shadowed Wilderness" [Woods] Set.RedSunrise

shadowedWilderness_176 :: CardDef
shadowedWilderness_176 =
  location_ "z-circus-ex-mortis-176" "Shadowed Wilderness" [Woods] Set.RedSunrise

shadowedWilderness_177 :: CardDef
shadowedWilderness_177 =
  location_ "z-circus-ex-mortis-177" "Shadowed Wilderness" [Woods] Set.RedSunrise

shadowedWilderness_178 :: CardDef
shadowedWilderness_178 =
  location_ "z-circus-ex-mortis-178" "Shadowed Wilderness" [Woods] Set.RedSunrise

shadowedWilderness_179 :: CardDef
shadowedWilderness_179 =
  location_ "z-circus-ex-mortis-179" "Shadowed Wilderness" [Woods] Set.RedSunrise

-- thousand_to_one
silentClearing :: CardDef
silentClearing =
  location_ "z-circus-ex-mortis-195" "Silent Clearing" [Woods] Set.ThousandToOne

primalForest :: CardDef
primalForest =
  victory 1 $ location_ "z-circus-ex-mortis-196" "Primal Forest" [Woods, Tainted] Set.ThousandToOne

highThicket :: CardDef
highThicket =
  location_ "z-circus-ex-mortis-197" "High Thicket" [Woods, Tainted] Set.ThousandToOne

sparseWoodland :: CardDef
sparseWoodland =
  location_ "z-circus-ex-mortis-198" "Sparse Woodland" [Woods, Tainted] Set.ThousandToOne

mossyGlen :: CardDef
mossyGlen =
  location_ "z-circus-ex-mortis-199" "Mossy Glen" [Woods] Set.ThousandToOne

fallenCopse :: CardDef
fallenCopse =
  location_ "z-circus-ex-mortis-200" "Fallen Copse" [Woods] Set.ThousandToOne

-- circus_grounds
animalCages :: CardDef
animalCages =
  victory 1 $ location_ "z-circus-ex-mortis-219" "Animal Cages" [NewMoonCircus] Set.CircusGrounds

carousel :: CardDef
carousel =
  location_ "z-circus-ex-mortis-220" "Carousel" [NewMoonCircus] Set.CircusGrounds

gamesGallery :: CardDef
gamesGallery =
  location_ "z-circus-ex-mortis-221" "Games Gallery" [NewMoonCircus] Set.CircusGrounds

performerTrailers :: CardDef
performerTrailers =
  victory 1 $ location_ "z-circus-ex-mortis-222" "Performer Trailers" [NewMoonCircus] Set.CircusGrounds

theBigTopFirstRing :: CardDef
theBigTopFirstRing =
  location_ "z-circus-ex-mortis-223" ("The Big Top" <:> "First Ring") [NewMoonCircus] Set.CircusGrounds

theBigTopSecondRing :: CardDef
theBigTopSecondRing =
  location_ "z-circus-ex-mortis-224" ("The Big Top" <:> "Second Ring") [NewMoonCircus] Set.CircusGrounds

theBigTopThirdRing :: CardDef
theBigTopThirdRing =
  location_ "z-circus-ex-mortis-225" ("The Big Top" <:> "Third Ring") [NewMoonCircus] Set.CircusGrounds
