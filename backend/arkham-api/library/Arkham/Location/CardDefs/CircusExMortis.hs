module Arkham.Location.CardDefs.CircusExMortis where

import Arkham.Location.CardDefs.Import

-- one_night_only
circusGatesPathToFreedomCircusExMortis :: CardDef
circusGatesPathToFreedomCircusExMortis =
  location_ "z-circus-ex-mortis-011" ("Circus Gates" <:> "Path to Freedom") [NewMoonCircus] CircusExMortisOneNightOnly

-- the_primrose_path
forestPassageCircusExMortis :: CardDef
forestPassageCircusExMortis =
  location_ "z-circus-ex-mortis-022" "Forest Passage" [Path] CircusExMortisThePrimrosePath

remoteCabinCircusExMortis :: CardDef
remoteCabinCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-023" "Remote Cabin" [Woods] CircusExMortisThePrimrosePath

woodlandOverlookCircusExMortis :: CardDef
woodlandOverlookCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-024" "Woodland Overlook" [Woods] CircusExMortisThePrimrosePath

circusEncampmentCircusExMortis :: CardDef
circusEncampmentCircusExMortis =
  location_ "z-circus-ex-mortis-025" "Circus Encampment" [Clearing] CircusExMortisThePrimrosePath

moonlitForestSmolderingCampfireCircusExMortis :: CardDef
moonlitForestSmolderingCampfireCircusExMortis =
  location_ "z-circus-ex-mortis-026" ("Moonlit Forest" <:> "Smoldering Campfire") [Woods] CircusExMortisThePrimrosePath

moonlitForestQuietValleyCircusExMortis :: CardDef
moonlitForestQuietValleyCircusExMortis =
  location_ "z-circus-ex-mortis-027" ("Moonlit Forest" <:> "Quiet Valley") [Woods] CircusExMortisThePrimrosePath

moonlitForestShallowRiverCircusExMortis :: CardDef
moonlitForestShallowRiverCircusExMortis =
  location_ "z-circus-ex-mortis-028" ("Moonlit Forest" <:> "Shallow River") [Woods] CircusExMortisThePrimrosePath

moonlitForestGlassyLakeCircusExMortis :: CardDef
moonlitForestGlassyLakeCircusExMortis =
  location_ "z-circus-ex-mortis-029" ("Moonlit Forest" <:> "Glassy Lake") [Woods] CircusExMortisThePrimrosePath

moonlitForestCircularGroveCircusExMortis :: CardDef
moonlitForestCircularGroveCircusExMortis =
  location_ "z-circus-ex-mortis-031" ("Moonlit Forest" <:> "Circular Grove") [Woods] CircusExMortisThePrimrosePath

moonlitForestMistyMarshCircusExMortis :: CardDef
moonlitForestMistyMarshCircusExMortis =
  location_ "z-circus-ex-mortis-032" ("Moonlit Forest" <:> "Misty Marsh") [Woods] CircusExMortisThePrimrosePath

moonlitForestShadowedPathCircusExMortis :: CardDef
moonlitForestShadowedPathCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-033" ("Moonlit Forest" <:> "Shadowed Path") [Woods] CircusExMortisThePrimrosePath

moonlitForestFogBankCircusExMortis :: CardDef
moonlitForestFogBankCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-034" ("Moonlit Forest" <:> "Fog Bank") [Woods] CircusExMortisThePrimrosePath

moonlitForestLabyrinthOfTreesCircusExMortis :: CardDef
moonlitForestLabyrinthOfTreesCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-035" ("Moonlit Forest" <:> "Labyrinth of Trees") [Woods] CircusExMortisThePrimrosePath

moonlitForestDeadGroveCircusExMortis :: CardDef
moonlitForestDeadGroveCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-036" ("Moonlit Forest" <:> "Dead Grove") [Woods] CircusExMortisThePrimrosePath

-- harm_s_way
ringmastersTrailerCircusExMortis :: CardDef
ringmastersTrailerCircusExMortis =
  location_ "z-circus-ex-mortis-049" "Ringmaster's Trailer" [Camp] CircusExMortisHarmsWay

crowdedRowCircusExMortis_050 :: CardDef
crowdedRowCircusExMortis_050 =
  location_ "z-circus-ex-mortis-050" "Crowded Row" [Camp] CircusExMortisHarmsWay

crowdedRowCircusExMortis_051 :: CardDef
crowdedRowCircusExMortis_051 =
  location_ "z-circus-ex-mortis-051" "Crowded Row" [Camp] CircusExMortisHarmsWay

crowdedRowCircusExMortis_052 :: CardDef
crowdedRowCircusExMortis_052 =
  location_ "z-circus-ex-mortis-052" "Crowded Row" [Camp] CircusExMortisHarmsWay

crowdedRowCircusExMortis_053 :: CardDef
crowdedRowCircusExMortis_053 =
  location_ "z-circus-ex-mortis-053" "Crowded Row" [Camp] CircusExMortisHarmsWay

secludedTentCircusExMortis_054 :: CardDef
secludedTentCircusExMortis_054 =
  location_ "z-circus-ex-mortis-054" "Secluded Tent" [Camp] CircusExMortisHarmsWay

secludedTentCircusExMortis_055 :: CardDef
secludedTentCircusExMortis_055 =
  location_ "z-circus-ex-mortis-055" "Secluded Tent" [Camp] CircusExMortisHarmsWay

secludedTentCircusExMortis_056 :: CardDef
secludedTentCircusExMortis_056 =
  location_ "z-circus-ex-mortis-056" "Secluded Tent" [Camp] CircusExMortisHarmsWay

secludedTentCircusExMortis_057 :: CardDef
secludedTentCircusExMortis_057 =
  location_ "z-circus-ex-mortis-057" "Secluded Tent" [Camp] CircusExMortisHarmsWay

campOutskirtsGuardedCloselyCircusExMortis :: CardDef
campOutskirtsGuardedCloselyCircusExMortis =
  location_ "z-circus-ex-mortis-058" ("Camp Outskirts" <:> "Guarded Closely") [Woods] CircusExMortisHarmsWay

campOutskirtsQuietForNowCircusExMortis :: CardDef
campOutskirtsQuietForNowCircusExMortis =
  location_ "z-circus-ex-mortis-059" ("Camp Outskirts" <:> "Quiet, For Now") [Woods] CircusExMortisHarmsWay

-- all_points_west
cabooseCircusExMortis :: CardDef
cabooseCircusExMortis =
  location_ "z-circus-ex-mortis-083" "Caboose" [Train] CircusExMortisAllPointsWest

locomotiveEngineCircusExMortis :: CardDef
locomotiveEngineCircusExMortis =
  location_ "z-circus-ex-mortis-084" "Locomotive Engine" [Train] CircusExMortisAllPointsWest

boxcarCircusExMortis :: CardDef
boxcarCircusExMortis =
  location_ "z-circus-ex-mortis-085" "Boxcar" [Train, FreightCar] CircusExMortisAllPointsWest

flatcarCircusExMortis :: CardDef
flatcarCircusExMortis =
  location_ "z-circus-ex-mortis-086" "Flatcar" [Train, FreightCar] CircusExMortisAllPointsWest

gondolaCarCircusExMortis :: CardDef
gondolaCarCircusExMortis =
  location_ "z-circus-ex-mortis-087" "Gondola Car" [Train, FreightCar] CircusExMortisAllPointsWest

stockCarCircusExMortis :: CardDef
stockCarCircusExMortis =
  location_ "z-circus-ex-mortis-088" "Stock Car" [Train, FreightCar] CircusExMortisAllPointsWest

tankCarCircusExMortis :: CardDef
tankCarCircusExMortis =
  location_ "z-circus-ex-mortis-089" "Tank Car" [Train, FreightCar] CircusExMortisAllPointsWest

coalHopperCarCircusExMortis :: CardDef
coalHopperCarCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-090" "Coal Hopper Car" [Train, SpecialCar] CircusExMortisAllPointsWest

craneCarCircusExMortis :: CardDef
craneCarCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-091" "Crane Car" [Train, SpecialCar] CircusExMortisAllPointsWest

mailCarCircusExMortis :: CardDef
mailCarCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-092" "Mail Car" [Train, SpecialCar] CircusExMortisAllPointsWest

refrigeratorCarCircusExMortis :: CardDef
refrigeratorCarCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-093" "Refrigerator Car" [Train, SpecialCar] CircusExMortisAllPointsWest

reinforcedCarCircusExMortis :: CardDef
reinforcedCarCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-094" "Reinforced Car" [Train, SpecialCar] CircusExMortisAllPointsWest

circusEngineCircusExMortis :: CardDef
circusEngineCircusExMortis =
  location_ "z-circus-ex-mortis-095" "Circus Engine" [CircusTrain] CircusExMortisAllPointsWest

exoticAnimalCarCircusExMortis :: CardDef
exoticAnimalCarCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-096" "Exotic Animal Car" [CircusTrain] CircusExMortisAllPointsWest

performersCarCircusExMortis :: CardDef
performersCarCircusExMortis =
  location_ "z-circus-ex-mortis-097" "Performers' Car" [CircusTrain] CircusExMortisAllPointsWest

-- piper_at_the_gates_of_dawn
circusGatesDoorwayToDoomCircusExMortis :: CardDef
circusGatesDoorwayToDoomCircusExMortis =
  location_ "z-circus-ex-mortis-119" ("Circus Gates" <:> "Doorway to Doom") [NewMoonCircus] CircusExMortisPiperAtTheGatesOfDawn

-- bacchanalia
vestibuleCircusExMortis :: CardDef
vestibuleCircusExMortis =
  location_ "z-circus-ex-mortis-131" "Vestibule" [LiberPater] CircusExMortisBacchanalia

banquetHallCircusExMortis :: CardDef
banquetHallCircusExMortis =
  location_ "z-circus-ex-mortis-132" "Banquet Hall" [LiberPater] CircusExMortisBacchanalia

statuaryGardensCircusExMortis :: CardDef
statuaryGardensCircusExMortis =
  location_ "z-circus-ex-mortis-133" "Statuary Gardens" [LiberPater] CircusExMortisBacchanalia

privateParlorCircusExMortis :: CardDef
privateParlorCircusExMortis =
  location_ "z-circus-ex-mortis-134" "Private Parlor" [LiberPater] CircusExMortisBacchanalia

collectionHallCircusExMortis :: CardDef
collectionHallCircusExMortis =
  location_ "z-circus-ex-mortis-135" "Collection Hall" [LiberPater] CircusExMortisBacchanalia

upperBalconyCircusExMortis :: CardDef
upperBalconyCircusExMortis =
  location_ "z-circus-ex-mortis-136" "Upper Balcony" [LiberPater] CircusExMortisBacchanalia

hiddenDungeonCircusExMortis :: CardDef
hiddenDungeonCircusExMortis =
  location_ "z-circus-ex-mortis-137" "Hidden Dungeon" [LiberPater, Restricted] CircusExMortisBacchanalia

manorCellarsCircusExMortis :: CardDef
manorCellarsCircusExMortis =
  location_ "z-circus-ex-mortis-138" "Manor Cellars" [LiberPater, Restricted] CircusExMortisBacchanalia

savageAltarCircusExMortis :: CardDef
savageAltarCircusExMortis =
  victory 2 $ location_ "z-circus-ex-mortis-139" "Savage Altar" [LiberPater, Restricted] CircusExMortisBacchanalia

-- red_sunrise
forgottenTrailCircusExMortis :: CardDef
forgottenTrailCircusExMortis =
  location_ "z-circus-ex-mortis-162" "Forgotten Trail" [Woods] CircusExMortisRedSunrise

ritualClearingCircusExMortis :: CardDef
ritualClearingCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-163" "Ritual Clearing" [Woods] CircusExMortisRedSunrise

foothillSlopeCircusExMortis_164 :: CardDef
foothillSlopeCircusExMortis_164 =
  location_ "z-circus-ex-mortis-164" "Foothill Slope" [Woods] CircusExMortisRedSunrise

foothillSlopeCircusExMortis_165 :: CardDef
foothillSlopeCircusExMortis_165 =
  location_ "z-circus-ex-mortis-165" "Foothill Slope" [Woods] CircusExMortisRedSunrise

foothillSlopeCircusExMortis_166 :: CardDef
foothillSlopeCircusExMortis_166 =
  location_ "z-circus-ex-mortis-166" "Foothill Slope" [Woods] CircusExMortisRedSunrise

foothillSlopeCircusExMortis_167 :: CardDef
foothillSlopeCircusExMortis_167 =
  location_ "z-circus-ex-mortis-167" "Foothill Slope" [Woods] CircusExMortisRedSunrise

mountainStreamCircusExMortis_168 :: CardDef
mountainStreamCircusExMortis_168 =
  location_ "z-circus-ex-mortis-168" "Mountain Stream" [Woods] CircusExMortisRedSunrise

mountainStreamCircusExMortis_169 :: CardDef
mountainStreamCircusExMortis_169 =
  location_ "z-circus-ex-mortis-169" "Mountain Stream" [Woods] CircusExMortisRedSunrise

mountainStreamCircusExMortis_170 :: CardDef
mountainStreamCircusExMortis_170 =
  location_ "z-circus-ex-mortis-170" "Mountain Stream" [Woods] CircusExMortisRedSunrise

mountainStreamCircusExMortis_171 :: CardDef
mountainStreamCircusExMortis_171 =
  location_ "z-circus-ex-mortis-171" "Mountain Stream" [Woods] CircusExMortisRedSunrise

openForestCircusExMortis_172 :: CardDef
openForestCircusExMortis_172 =
  location_ "z-circus-ex-mortis-172" "Open Forest" [Woods] CircusExMortisRedSunrise

openForestCircusExMortis_173 :: CardDef
openForestCircusExMortis_173 =
  location_ "z-circus-ex-mortis-173" "Open Forest" [Woods] CircusExMortisRedSunrise

openForestCircusExMortis_174 :: CardDef
openForestCircusExMortis_174 =
  location_ "z-circus-ex-mortis-174" "Open Forest" [Woods] CircusExMortisRedSunrise

shadowedWildernessCircusExMortis_175 :: CardDef
shadowedWildernessCircusExMortis_175 =
  location_ "z-circus-ex-mortis-175" "Shadowed Wilderness" [Woods] CircusExMortisRedSunrise

shadowedWildernessCircusExMortis_176 :: CardDef
shadowedWildernessCircusExMortis_176 =
  location_ "z-circus-ex-mortis-176" "Shadowed Wilderness" [Woods] CircusExMortisRedSunrise

shadowedWildernessCircusExMortis_177 :: CardDef
shadowedWildernessCircusExMortis_177 =
  location_ "z-circus-ex-mortis-177" "Shadowed Wilderness" [Woods] CircusExMortisRedSunrise

shadowedWildernessCircusExMortis_178 :: CardDef
shadowedWildernessCircusExMortis_178 =
  location_ "z-circus-ex-mortis-178" "Shadowed Wilderness" [Woods] CircusExMortisRedSunrise

shadowedWildernessCircusExMortis_179 :: CardDef
shadowedWildernessCircusExMortis_179 =
  location_ "z-circus-ex-mortis-179" "Shadowed Wilderness" [Woods] CircusExMortisRedSunrise

-- thousand_to_one
silentClearingCircusExMortis :: CardDef
silentClearingCircusExMortis =
  location_ "z-circus-ex-mortis-195" "Silent Clearing" [Woods] CircusExMortisThousandToOne

primalForestCircusExMortis :: CardDef
primalForestCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-196" "Primal Forest" [Woods, Tainted] CircusExMortisThousandToOne

highThicketCircusExMortis :: CardDef
highThicketCircusExMortis =
  location_ "z-circus-ex-mortis-197" "High Thicket" [Woods, Tainted] CircusExMortisThousandToOne

sparseWoodlandCircusExMortis :: CardDef
sparseWoodlandCircusExMortis =
  location_ "z-circus-ex-mortis-198" "Sparse Woodland" [Woods, Tainted] CircusExMortisThousandToOne

mossyGlenCircusExMortis :: CardDef
mossyGlenCircusExMortis =
  location_ "z-circus-ex-mortis-199" "Mossy Glen" [Woods] CircusExMortisThousandToOne

fallenCopseCircusExMortis :: CardDef
fallenCopseCircusExMortis =
  location_ "z-circus-ex-mortis-200" "Fallen Copse" [Woods] CircusExMortisThousandToOne

-- circus_grounds
animalCagesCircusExMortis :: CardDef
animalCagesCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-219" "Animal Cages" [NewMoonCircus] CircusExMortisCircusGrounds

carouselCircusExMortis :: CardDef
carouselCircusExMortis =
  location_ "z-circus-ex-mortis-220" "Carousel" [NewMoonCircus] CircusExMortisCircusGrounds

gamesGalleryCircusExMortis :: CardDef
gamesGalleryCircusExMortis =
  location_ "z-circus-ex-mortis-221" "Games Gallery" [NewMoonCircus] CircusExMortisCircusGrounds

performerTrailersCircusExMortis :: CardDef
performerTrailersCircusExMortis =
  victory 1 $ location_ "z-circus-ex-mortis-222" "Performer Trailers" [NewMoonCircus] CircusExMortisCircusGrounds

theBigTopFirstRingCircusExMortis :: CardDef
theBigTopFirstRingCircusExMortis =
  location_ "z-circus-ex-mortis-223" ("The Big Top" <:> "First Ring") [NewMoonCircus] CircusExMortisCircusGrounds

theBigTopSecondRingCircusExMortis :: CardDef
theBigTopSecondRingCircusExMortis =
  location_ "z-circus-ex-mortis-224" ("The Big Top" <:> "Second Ring") [NewMoonCircus] CircusExMortisCircusGrounds

theBigTopThirdRingCircusExMortis :: CardDef
theBigTopThirdRingCircusExMortis =
  location_ "z-circus-ex-mortis-225" ("The Big Top" <:> "Third Ring") [NewMoonCircus] CircusExMortisCircusGrounds
