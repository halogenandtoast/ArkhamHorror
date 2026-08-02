module Arkham.Location.CardDefs.TheDrownedCity where

import Arkham.Location.CardDefs.Import

-- One Last Job
tillinghastEsoterica :: CardDef
tillinghastEsoterica =
  location_ "11509" ("Tillinghast Esoterica" <:> "Assorted Curiosities") [] OneLastJob

-- Gang hideouts: the reverse sides of the two "Questioning the Gangs" act copies.
hibbsRoadhouse :: CardDef
hibbsRoadhouse =
  victory 1 $ otherSideIs "11504" $ location_ "11504b" "Hibb's Roadhouse" [Arkham, Front] OneLastJob

laBellaLunaTheDrownedCity :: CardDef
laBellaLunaTheDrownedCity =
  victory 1 $ otherSideIs "11505" $ location_ "11505b" "La Bella Luna" [Arkham, Front] OneLastJob

-- The Western Wall
treacherousPathSlickSteps :: CardDef
treacherousPathSlickSteps =
  locationWithUnrevealed_
    "11521"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Slick Steps")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathErodedShelf :: CardDef
treacherousPathErodedShelf =
  locationWithUnrevealed_
    "11522"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Eroded Shelf")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathPrecariousClimb :: CardDef
treacherousPathPrecariousClimb =
  locationWithUnrevealed_
    "11523"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Precarious Climb")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathDeadlyPass :: CardDef
treacherousPathDeadlyPass =
  locationWithUnrevealed_
    "11524"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Deadly Pass")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathShallowDen :: CardDef
treacherousPathShallowDen =
  locationWithUnrevealed_
    "11525"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Shallow Den")
    [Rlyeh, Walkway]
    TheWesternWall

sunkenStairway :: CardDef
sunkenStairway =
  locationWithUnrevealed_
    "11526"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    "Sunken Stairway"
    [Rlyeh, Walkway]
    TheWesternWall

drownedShanty :: CardDef
drownedShanty =
  locationWithUnrevealed_
    "11527"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    "Drowned Shanty"
    [Rlyeh, Walkway]
    TheWesternWall

shatteredRuins :: CardDef
shatteredRuins =
  victory 1
    $ locationWithUnrevealed_
      "11528"
      "Treacherous Paths"
      [Rlyeh, Walkway]
      "Shattered Ruins"
      [Rlyeh, Walkway, Glyph]
      TheWesternWall

obsidianFoundations :: CardDef
obsidianFoundations =
  locationWithUnrevealed_
    "11529"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    "Obsidian Foundations"
    [Rlyeh]
    TheWesternWall

westernWall_11530 :: CardDef
westernWall_11530 =
  location_ "11530" "Western Wall" [Rlyeh] TheWesternWall

underseaVault :: CardDef
underseaVault =
  storyOnBack' "11532b"
    $ victory 1
    $ location_ "11532" "Undersea Vault" [Vault, Glyph] TheWesternWall

-- The Drowned Quarter

-- | Every Sea Floor location shares the same unrevealed back, "Sea Floor".
seaFloor :: CardCode -> Name -> CardDef
seaFloor cardCode name =
  locationWithUnrevealed_ cardCode "Sea Floor" [Seafloor] name [Seafloor] TheDrownedQuarter

-- Barrier Core has no unrevealed side; it flips between (Inactive) and (Active).
barrierCoreInactive :: CardDef
barrierCoreInactive =
  otherSideIs "11540b"
    $ location_ "11540" ("Barrier Core" <:> "Inactive") [Seafloor, Central] TheDrownedQuarter

barrierCoreActive :: CardDef
barrierCoreActive =
  otherSideIs "11540"
    $ location_ "11540b" ("Barrier Core" <:> "Active") [Seafloor, Central] TheDrownedQuarter

abyssalTrench :: CardDef
abyssalTrench =
  seaFloor "11541" "Abyssal Trench"

drownedAcropolisEphemeralRuins :: CardDef
drownedAcropolisEphemeralRuins =
  seaFloor "11542" ("Drowned Acropolis" <:> "Ephemeral Ruins")

drownedAcropolisCollapsedRuins :: CardDef
drownedAcropolisCollapsedRuins =
  seaFloor "11543" ("Drowned Acropolis" <:> "Collapsed Ruins")

blastedRuinsSunkenCircle :: CardDef
blastedRuinsSunkenCircle =
  seaFloor "11544" ("Blasted Ruins" <:> "Sunken Circle")

blastedRuinsCrumblingEdifices :: CardDef
blastedRuinsCrumblingEdifices =
  seaFloor "11545" ("Blasted Ruins" <:> "Crumbling Edifices")

coralReefStatuaryGarden :: CardDef
coralReefStatuaryGarden =
  victory 1 $ seaFloor "11546" ("Coral Reef" <:> "Statuary Garden")

coralReefFeedingGrounds :: CardDef
coralReefFeedingGrounds =
  victory 1 $ seaFloor "11547" ("Coral Reef" <:> "Feeding Grounds")

ancientGallery :: CardDef
ancientGallery =
  victory 1 $ seaFloor "11548" "Ancient Gallery"

-- The Apiary
apiaryEntranceBeckoningLight :: CardDef
apiaryEntranceBeckoningLight =
  otherSideIs "11559b"
    $ location
      "11559"
      ("Apiary Entrance" <:> "Beckoning Light")
      [Apiary, Central]
      Diamond
      [Moon, Equals, Spade, Circle, Square]
      TheApiary

apiaryEntranceDangerousExit :: CardDef
apiaryEntranceDangerousExit =
  otherSideIs "11559"
    $ location
      "11559b"
      ("Apiary Entrance" <:> "Dangerous Exit")
      [Apiary, Central]
      Diamond
      [Moon, Equals, Spade, Circle, Square]
      TheApiary

fleshyPathsEasternBurrows :: CardDef
fleshyPathsEasternBurrows =
  singleSided
    $ location "11560" ("Fleshy Paths" <:> "Eastern Burrows") [Apiary] Moon [Diamond] TheApiary

fleshyPathsWesternBurrows :: CardDef
fleshyPathsWesternBurrows =
  singleSided
    $ victory 1
    $ location "11561" ("Fleshy Paths" <:> "Western Burrows") [Apiary, Glyph] Equals [Diamond] TheApiary

growingFields :: CardDef
growingFields =
  singleSided
    $ location "11562" "Growing Fields" [Apiary] Spade [Diamond, Star] TheApiary

churningChasm :: CardDef
churningChasm =
  singleSided
    $ location "11563" "Churning Chasm" [Apiary] Droplet [Heart] TheApiary

corruptedVault :: CardDef
corruptedVault =
  singleSided
    $ victory 1
    $ location "11564" "Corrupted Vault" [Apiary, Glyph] Triangle [Circle] TheApiary

luminousTunnels :: CardDef
luminousTunnels =
  singleSided
    $ location "11565" "Luminous Tunnels" [Apiary, Enclave] Circle [Diamond, Heart, Triangle] TheApiary

spawningGrounds :: CardDef
spawningGrounds =
  singleSided
    $ location "11566" "Spawning Grounds" [Apiary, Enclave] Heart [Circle, Square, Droplet] TheApiary

lostCampsite :: CardDef
lostCampsite =
  singleSided
    $ victory 1
    $ location "11567" "Lost Campsite" [Apiary, Enclave, Sanctum] Square [Heart] TheApiary

graspingCorridor :: CardDef
graspingCorridor =
  singleSided
    $ location "11569" "Grasping Corridor" [Apiary, Nest] Circle [Diamond, Heart, Triangle] TheApiary

starvingCorridor :: CardDef
starvingCorridor =
  singleSided
    $ location "11570" "Starving Corridor" [Apiary, Nest] Heart [Circle, Square, Droplet] TheApiary

acidicCoelom :: CardDef
acidicCoelom =
  singleSided
    $ victory 1
    $ location "11571" "Acidic Coelom" [Apiary, Nest, Sanctum] Square [Heart, Diamond] TheApiary

centralChamber :: CardDef
centralChamber =
  location_ "11572" "Central Chamber" [Apiary, Nest, Central] TheApiary

-- TODO: back side of double-sided card (11579b)
hiddenVault :: CardDef
hiddenVault =
  storyOnBack' "11579b"
    $ victory 1
    $ location "11579" "Hidden Vault" [Apiary, Glyph] Star [Spade] TheApiary

-- The Grand Vault
theGreatStair :: CardDef
theGreatStair =
  location_ "11593" "The Great Stair" [Rlyeh] TheGrandVault

movingPlatformObservationStation :: CardDef
movingPlatformObservationStation =
  location_ "11594" ("Moving Platform" <:> "Observation Station") [Rlyeh] TheGrandVault

coreOfTheVaultHeartOfTheMachine :: CardDef
coreOfTheVaultHeartOfTheMachine =
  location_
    "11595"
    ("Core of the Vault" <:> "Heart of the Machine")
    [Rlyeh, Sanctum, Glyph]
    TheGrandVault

{- | Every Vault Chamber location shares the same unrevealed back, "Vault Chamber".
Setup shuffles them together facedown, so the unrevealed side keeps the @Vault@
trait that the scenario's flood and activation effects select on.
-}
vaultChamber :: CardCode -> Name -> [Trait] -> CardDef
vaultChamber cardCode name traits =
  locationWithUnrevealed_ cardCode "Vault Chamber" [Rlyeh, Vault] name traits TheGrandVault

shroudedCistern :: CardDef
shroudedCistern =
  quantity 2 $ vaultChamber "11596" "Shrouded Cistern" [Rlyeh, Vault]

chamberOfRecordsArm :: CardDef
chamberOfRecordsArm =
  victory 1 $ vaultChamber "11598" "Chamber of Records" [Rlyeh, Vault, Glyph]

chamberOfRecordsEarth :: CardDef
chamberOfRecordsEarth =
  victory 1 $ vaultChamber "11599" "Chamber of Records" [Rlyeh, Vault, Glyph]

otherworldlyMechanismsObsidianBulwark :: CardDef
otherworldlyMechanismsObsidianBulwark =
  vaultChamber "11600" ("Otherworldly Mechanisms" <:> "Obsidian Bulwark") [Rlyeh, Vault]

otherworldlyMechanismsSluiceControl :: CardDef
otherworldlyMechanismsSluiceControl =
  vaultChamber "11601" ("Otherworldly Mechanisms" <:> "Sluice Control") [Rlyeh, Vault]

otherworldlyMechanismsGrimeCoveredGears :: CardDef
otherworldlyMechanismsGrimeCoveredGears =
  vaultChamber "11602" ("Otherworldly Mechanisms" <:> "Grime-Covered Gears") [Rlyeh, Vault]

otherworldlyMechanismsInscrutableApparatus :: CardDef
otherworldlyMechanismsInscrutableApparatus =
  vaultChamber "11603" ("Otherworldly Mechanisms" <:> "Inscrutable Apparatus") [Rlyeh, Vault]

-- | Enters play on its unrevealed "Sealed Chamber" side; the act flips it.
chamberOfTheTabletUnsealed :: CardDef
chamberOfTheTabletUnsealed =
  locationWithUnrevealed_
    "11604"
    "Sealed Chamber"
    [Rlyeh]
    ("Chamber of the Tablet" <:> "Unsealed")
    [Rlyeh]
    TheGrandVault

-- Court of the Ancients
westAntechamber :: CardDef
westAntechamber =
  location_ "11619" "West Antechamber" [Rlyeh] CourtOfTheAncients

-- East Antechamber and Ancient Altar are the only Court locations with connection
-- icons; every other location in the scenario is reachable only via the Great
-- Lift or a card effect.
eastAntechamber :: CardDef
eastAntechamber =
  location "11620" "East Antechamber" [Rlyeh] Heart [Plus] CourtOfTheAncients

twistingCatwalks :: CardDef
twistingCatwalks =
  location_ "11621" ("Twisting Catwalks" <:> "Western Rise") [Rlyeh] CourtOfTheAncients

greatLiftInactive :: CardDef
greatLiftInactive =
  otherSideIs "11622b"
    $ location_ "11622" ("Great Lift" <:> "Inactive") [Rlyeh, Lift] CourtOfTheAncients

greatLiftActive :: CardDef
greatLiftActive =
  otherSideIs "11622"
    $ location_ "11622b" ("Great Lift" <:> "Active") [Rlyeh, Lift] CourtOfTheAncients

ancientAltar :: CardDef
ancientAltar =
  victory 1 $ location "11623" "Ancient Altar" [Rlyeh] Plus [Heart] CourtOfTheAncients

-- The six archives all share the "Crumbling Archives" unrevealed back; setup
-- shuffles them, removes one at random, and places the rest face down.
crumblingArchives :: Name
crumblingArchives = "Crumbling Archives"

ringLibraryArchiveOfTheStars :: CardDef
ringLibraryArchiveOfTheStars =
  locationWithUnrevealed_
    "11624"
    crumblingArchives
    [Rlyeh]
    ("Ring Library" <:> "Archive of the Stars")
    [Rlyeh, Passageway]
    CourtOfTheAncients

ringLibraryArchiveOfTheAncients :: CardDef
ringLibraryArchiveOfTheAncients =
  locationWithUnrevealed_
    "11625"
    crumblingArchives
    [Rlyeh]
    ("Ring Library" <:> "Archive of the Ancients")
    [Rlyeh]
    CourtOfTheAncients

loftyWalkwayArchiveOfDreams :: CardDef
loftyWalkwayArchiveOfDreams =
  locationWithUnrevealed_
    "11626"
    crumblingArchives
    [Rlyeh]
    ("Lofty Walkway" <:> "Archive of Dreams")
    [Rlyeh, Passageway]
    CourtOfTheAncients

loftyWalkwayArchiveOfConflict :: CardDef
loftyWalkwayArchiveOfConflict =
  locationWithUnrevealed_
    "11627"
    crumblingArchives
    [Rlyeh]
    ("Lofty Walkway" <:> "Archive of Conflict")
    [Rlyeh]
    CourtOfTheAncients

luminousArchivesArchiveOfHistory :: CardDef
luminousArchivesArchiveOfHistory =
  locationWithUnrevealed_
    "11628"
    crumblingArchives
    [Rlyeh]
    ("Luminous Archives" <:> "Archive of History")
    [Rlyeh, Passageway]
    CourtOfTheAncients

luminousArchivesArchiveOfMemory :: CardDef
luminousArchivesArchiveOfMemory =
  locationWithUnrevealed_
    "11629"
    crumblingArchives
    [Rlyeh]
    ("Luminous Archives" <:> "Archive of Memory")
    [Rlyeh]
    CourtOfTheAncients

-- Obsidian Canyons

{- | Every Summit location shares one unrevealed back, so they can be shuffled
into the Summit deck face-down and told apart only once revealed. R'lyeh Streets
is the exception: it is placed face-up during setup and never enters the deck.
-}
summit :: Name
summit = "Summit"

rlyehStreets :: CardDef
rlyehStreets =
  location_ "11648" "R'lyeh Streets" [Rlyeh, Central] ObsidianCanyons

centralSpire :: CardDef
centralSpire =
  locationWithUnrevealed_ "11649" summit [Summit] "Central Spire" [Rlyeh, Central] ObsidianCanyons

floatingSpire :: CardDef
floatingSpire =
  locationWithUnrevealed_ "11650" summit [Summit] "Floating Spire" [Rlyeh, Central] ObsidianCanyons

westernWall_11651 :: CardDef
westernWall_11651 =
  locationWithUnrevealed_ "11651" summit [Summit] "Western Wall" [Rlyeh, Central] ObsidianCanyons

ancientDome :: CardDef
ancientDome =
  locationWithUnrevealed_ "11652" summit [Summit] "Ancient Dome" [Rlyeh, Central] ObsidianCanyons

easternAthenaeum :: CardDef
easternAthenaeum =
  victory 1
    $ locationWithUnrevealed_
      "11653"
      summit
      [Summit]
      "Eastern Athenaeum"
      [Rlyeh, Summit, Glyph]
      ObsidianCanyons

westernAthenaeum :: CardDef
westernAthenaeum =
  victory 1
    $ locationWithUnrevealed_
      "11654"
      summit
      [Summit]
      "Western Athenaeum"
      [Rlyeh, Summit, Glyph]
      ObsidianCanyons

obsidianCliffs :: CardDef
obsidianCliffs =
  victory 1
    $ locationWithUnrevealed_ "11655" summit [Summit] "Obsidian Cliffs" [Rlyeh, Summit] ObsidianCanyons

suspendedReef :: CardDef
suspendedReef =
  quantity 3
    $ locationWithUnrevealed_ "11656" summit [Summit] "Suspended Reef" [Rlyeh, Summit] ObsidianCanyons

hangingShip :: CardDef
hangingShip =
  quantity 2 $ locationWithUnrevealed_ "11657" summit [Summit] "Hanging Ship" [Summit] ObsidianCanyons

ancientCanyons :: CardDef
ancientCanyons =
  locationWithUnrevealed_ "11658" summit [Summit] "Ancient Canyons" [Summit] ObsidianCanyons

dazzlingSkyline :: CardDef
dazzlingSkyline =
  quantity 3
    $ locationWithUnrevealed_ "11659" summit [Summit] "Dazzling Skyline" [Summit] ObsidianCanyons

aerialWaterfall :: CardDef
aerialWaterfall =
  locationWithUnrevealed_ "11660" summit [Summit] "Aerial Waterfall" [Summit] ObsidianCanyons

magneticSpires :: CardDef
magneticSpires =
  quantity 2
    $ locationWithUnrevealed_ "11661" summit [Summit] "Magnetic Spires" [Summit] ObsidianCanyons

-- TODO: back side of double-sided card (11662b)
glyphOrrery :: CardDef
glyphOrrery =
  storyOnBack' "11662b"
    $ victory 1
    $ location_ "11662" "Glyph Orrery" [Rlyeh, Summit, Glyph] ObsidianCanyons

-- Sepulchre of the Sleeper
dreamersRest :: CardDef
dreamersRest =
  location_ "11676" "Dreamer's Rest" [Rlyeh, Lair] SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfAmbition :: CardDef
sigilCarvedAlcoveStoryOfAmbition =
  location_
    "11677"
    ("Sigil-Carved Alcove" <:> "Story of Ambition")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfResilience :: CardDef
sigilCarvedAlcoveStoryOfResilience =
  location_
    "11678"
    ("Sigil-Carved Alcove" <:> "Story of Resilience")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfInfinity :: CardDef
sigilCarvedAlcoveStoryOfInfinity =
  location_
    "11679"
    ("Sigil-Carved Alcove" <:> "Story of Infinity")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfDefiance :: CardDef
sigilCarvedAlcoveStoryOfDefiance =
  location_
    "11680"
    ("Sigil-Carved Alcove" <:> "Story of Defiance")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfTheVoyage :: CardDef
sigilCarvedAlcoveStoryOfTheVoyage =
  location_
    "11681"
    ("Sigil-Carved Alcove" <:> "Story of the Voyage")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

-- The Doom of Arkham, Part I
tillinghastEsotericaEphemeralShop :: CardDef
tillinghastEsotericaEphemeralShop =
  singleSided
    $ victory 1
    $ location_
      "11685"
      ("Tillinghast Esoterica" <:> "Ephemeral Shop")
      [Sanctum, Extradimensional]
      TheDoomOfArkhamPartI

-- The Doom of Arkham, Part II
northsideTheDrownedCity :: CardDef
northsideTheDrownedCity =
  location_ "11692" ("Northside" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

downtown :: CardDef
downtown =
  location_ "11693" ("Downtown" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

easttownTheDrownedCity :: CardDef
easttownTheDrownedCity =
  location_ "11694" ("Easttown" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

miskatonicUniversityTheDrownedCity :: CardDef
miskatonicUniversityTheDrownedCity =
  location_ "11695" ("Miskatonic University" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

rivertownTheDrownedCity :: CardDef
rivertownTheDrownedCity =
  location_ "11696" ("Rivertown" <:> "Ruined") [Arkham, Central, Ruined] TheDoomOfArkhamPartII

stMarysHospitalTheDrownedCity :: CardDef
stMarysHospitalTheDrownedCity =
  location_ "11697" ("St. Mary's Hospital" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

southside :: CardDef
southside =
  location_ "11698" ("Southside" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

westernRooftops :: CardDef
westernRooftops =
  location_ "11699" "Western Rooftops" [Arkham, Rooftop] TheDoomOfArkhamPartII

easternRooftops :: CardDef
easternRooftops =
  location_ "11700" "Eastern Rooftops" [Arkham, Rooftop] TheDoomOfArkhamPartII
